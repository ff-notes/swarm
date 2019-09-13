{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main,
  )
where

import Control.Concurrent.STM (TChan, atomically, tryReadTChan)
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QTimer as QTimer
import Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import Graphics.UI.Qtah.Widgets.QTextEdit (QTextEdit)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import RON.Data (evalObjectState)
import qualified RON.Data.RGA as RGA
import RON.Data.RGA (RgaString)
import RON.Storage.Backend
  ( CollectionName,
    DocId (DocId),
    Document (Document, objectFrame),
    RawDocId,
  )
import qualified RON.Storage.FS as Storage
import RON.Storage.FS (Collection, loadDocument, modify, runStorage)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

theDoc :: DocId RgaString
theDoc = DocId "B3QCFGMHK1QJS-2005CRP400492"

dataDir :: FilePath
dataDir = "demo/data"

main :: IO ()
main = do
  progName <- getProgName
  storage <- Storage.newHandle dataDir
  dbChanges <- Storage.subscribe storage
  withApp $ \_ -> do
    window <- newMainWindow progName
    QWidget.show window
    do
      editor <- getEditor window
      connect_ editor QTextEdit.textChangedSignal $ saveTheText storage editor
    updateTheText storage window
    do
      always <- QTimer.new
      connect_ always QTimer.timeoutSignal $ tryUpdate storage window dbChanges
      QTimer.start always 0
    QCoreApplication.exec
  where
    withApp = withScopedPtr $ do
      args <- getArgs
      QApplication.new args

tryUpdate
  :: Storage.Handle -> QMainWindow -> TChan (CollectionName, RawDocId) -> IO ()
tryUpdate storage window dbChanges =
  atomically (tryReadTChan dbChanges) >>= \case
    Just (collection, docid) -> update storage window collection docid
    Nothing -> pure ()

update :: Storage.Handle -> QMainWindow -> CollectionName -> RawDocId -> IO ()
update storage window collection docid = case collection of
  "text"
    | DocId docid == theDoc -> updateTheText storage window
    | otherwise -> hPutStrLn stderr $ "update: unknown document " <> show docid
  _ -> hPutStrLn stderr $ "update: unknown document type " <> show docid

updateTheText :: Storage.Handle -> QMainWindow -> IO ()
updateTheText storage window = do
  text <-
    runStorage storage $ do
      Document {objectFrame} <- loadDocument theDoc
      evalObjectState objectFrame RGA.getList
  editor <- getEditor window
  QTextEdit.setPlainText editor text

getEditor :: QMainWindow -> IO QTextEdit
getEditor window = QTextEdit.downCast <$> QMainWindow.centralWidget window

newMainWindow :: String -> IO QMainWindow
newMainWindow progName = do
  window <- QMainWindow.new
  QWidget.setWindowTitle window progName
  QMainWindow.setCentralWidget window =<< QTextEdit.new
  pure window

instance Collection RgaString where
  collectionName = "text"

saveTheText :: Storage.Handle -> QTextEdit -> IO ()
saveTheText storage editor = do
  text <- QTextEdit.toPlainText editor
  runStorage storage $ modify theDoc $ RGA.edit text
