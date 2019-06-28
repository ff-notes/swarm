{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Brick (App (App), BrickEvent (AppEvent, VtyEvent), EventM,
                        Next, Widget, attrMap, continue, customMain, halt,
                        showFirstCursor, txt, (<=>))
import qualified Brick
import           Brick.BChan (BChan, newBChan, writeBChan)
import           Brick.Widgets.Border (border)
import           Brick.Widgets.Edit (Editor, applyEdit, editorText,
                                     getEditContents, handleEditorEvent,
                                     renderEditor)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Zipper (TextZipper, cursorPosition, moveCursor,
                                   textZipper)
import qualified Data.Text.Zipper as TextZipper
import           Graphics.Vty (Event (EvKey), Key (KEsc), defAttr,
                               defaultConfig, mkVty)
import           RON.Data (evalObjectState, execObjectState)
import           RON.Data.RGA (RgaString)
import qualified RON.Data.RGA as RGA
import           RON.Storage.Backend (DocId (DocId), Document (Document),
                                      createVersion, value)
import           RON.Storage.FS (loadDocument, runStorage)
import qualified RON.Storage.FS as Storage

import           Types ()

theDoc :: DocId RgaString
theDoc = DocId "B3QCFGMHK1QJS-2005CRP400492"

main :: IO ()
main = do
    let dataDir = "demo/data"
    h <- Storage.newHandle dataDir
    (document, text) <- runStorage h $ do
        document@Document{value = obj} <- loadDocument theDoc
        text <- evalObjectState obj RGA.getText
        pure (document, text)
    rhythm <- mkRhythm
    runUI rhythm (mkApp h) MyState{editor = mkEditor text, document}

mkEditor :: Text -> Editor Text ()
mkEditor = editorText () Nothing

mkRhythm :: IO (BChan ())
mkRhythm = do
    chan <- newBChan 1
    _ <- forkIO $ forever $ do
        writeBChan chan ()
        threadDelay 1000000
    pure chan

runUI :: BChan () -> App s () () -> s -> IO ()
runUI chan app initialState = do
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app initialState

data MyState = MyState{editor :: Editor Text (), document :: Document RgaString}

mkApp :: Storage.Handle -> App MyState () ()
mkApp storage = App
    { appAttrMap = const $ attrMap defAttr []
    , appChooseCursor = showFirstCursor
    , appDraw = draw
    , appHandleEvent = handleEvent storage
    , appStartEvent = pure
    }

draw :: MyState -> [Widget ()]
draw MyState{editor} =
    [   border (renderEditor (txt . Text.unlines) True editor)
    <=> txt "Esc -> exit"
    ]

handleEvent
    :: Storage.Handle -> MyState -> BrickEvent () () -> EventM () (Next MyState)
handleEvent storage state@MyState{editor} = \case
    VtyEvent ve -> case ve of
        EvKey KEsc [] -> do
            state' <- liftIO $ sync storage state False
            halt state'
        _ -> do
            editor' <- handleEditorEvent ve editor
            continue state{editor = editor'}
    AppEvent () -> do
        state' <- liftIO $ sync storage state True
        continue state'
    _ -> continue state

sync :: Storage.Handle -> MyState -> Bool -> IO MyState
sync storage state@MyState{editor, document} reload =
    runStorage storage $ do
        do  let Document{value = obj} = document
            obj' <-
                execObjectState obj $
                RGA.editText $ Text.unlines $ getEditContents editor
            createVersion (Just (theDoc, document)) obj'
        if reload then do
            document'@Document{value = obj} <- loadDocument theDoc
            text <- evalObjectState obj RGA.getText
            let editor' = applyEdit (replaceZipper text) editor
            pure MyState{editor = editor', document = document'}
        else
            pure state

-- | Replace content in zipper keeping cursor
-- TODO save RGA ids and keep position after RGA changes
replaceZipper :: Text -> TextZipper Text -> TextZipper Text
replaceZipper text zipper = moveCursorClosely originalPosition zipper' where
    zipper' = textZipper (Text.lines text) Nothing
    originalPosition = cursorPosition zipper

moveCursorClosely :: (Int, Int) -> TextZipper Text -> TextZipper Text
moveCursorClosely (row, col) tz
    | row < 0     = moveCursor (0       , 0   ) tz
    | row >= rows = moveCursor (rows - 1, 0   ) tz
    | col < 0     = moveCursor (row     , 0   ) tz
    | col > cols  = moveCursor (row     , cols) tz
    | otherwise   = moveCursor (row     , col ) tz
  where
    t = TextZipper.getText tz
    rows = length t
    cols = Text.length (t !! row)
