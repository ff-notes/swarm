{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A real-world file storage.
--
-- Typical usage:
--
-- @
-- import RON.Storage.FS as Storage
--
-- main = do
--     let dataDir = ".\/data\/"
--     h <- Storage.'newHandle' dataDir
--     'runStorage' h $ do
--         obj <- 'newObjectState' Note{active = True, text = "Write an example"}
--         'createDocument' obj
-- @
module RON.Storage.FS (
    module X,
    -- * Handle
    Handle,
    newHandle,
    newHandleWithReplicaId,
    -- * Storage
    Storage,
    runStorage,
    subscribeForever,
) where

import           RON.Prelude

import           Control.Concurrent.STM (TChan, atomically, dupTChan,
                                         newBroadcastTChanIO, readTChan,
                                         writeTChan)
import           Control.Monad (forever)
import           Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BSL
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           System.Directory (canonicalizePath, createDirectoryIfMissing,
                                   doesDirectoryExist, doesPathExist,
                                   listDirectory, removeFile, renameDirectory)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)
import           System.Random.TF (newTFGen)
import           System.Random.TF.Instances (random)

import           RON.Epoch (EpochClock, getCurrentEpochTime, runEpochClock)
import           RON.Error (Error, throwErrorString)
import           RON.Event (EpochTime, ReplicaClock, ReplicaId, advance,
                            applicationSpecific, getEvents, getPid)

import           RON.Storage as X
import           RON.Storage.Backend (DocId (DocId), MonadStorage, changeDocId,
                                      deleteVersion, getCollections,
                                      getDocumentVersions, getDocuments,
                                      loadVersionContent, saveVersionContent)

-- | Environment is the dataDir
newtype Storage a = Storage (ExceptT Error (ReaderT Handle EpochClock) a)
    deriving (Applicative, Functor, Monad, MonadError Error, MonadIO)

-- | Run a 'Storage' action
runStorage :: Handle -> Storage a -> IO a
runStorage h@Handle{hReplica, hClock} (Storage action) = do
    res <-
        runEpochClock hReplica hClock $
        (`runReaderT` h) $
        runExceptT action
    either throwIO pure res

instance ReplicaClock Storage where
    getPid    = Storage . lift $ lift getPid
    getEvents = Storage . lift . lift . getEvents
    advance   = Storage . lift . lift . advance

instance MonadStorage Storage where
    getCollections = Storage $ do
        Handle{hDataDir} <- ask
        liftIO $
            listDirectory hDataDir
            >>= filterM (doesDirectoryExist . (hDataDir </>))

    getDocuments :: forall doc. Collection doc => Storage [DocId doc]
    getDocuments = map DocId <$> listDirectoryIfExists (collectionName @doc)

    getDocumentVersions = listDirectoryIfExists . docDir

    saveVersionContent docid version content = do
        Storage $ do
            Handle{hDataDir} <- ask
            let docdir = hDataDir </> docDir docid
            liftIO $ do
                createDirectoryIfMissing True docdir
                BSL.writeFile (docdir </> version) content
        emitDocumentChanged docid

    loadVersionContent docid version = Storage $ do
        Handle{hDataDir} <- ask
        liftIO $ BSL.readFile $ hDataDir </> docDir docid </> version

    deleteVersion docid version = Storage $ do
        Handle{hDataDir} <- ask
        liftIO $ do
            let file = hDataDir </> docDir docid </> version
            removeFile file
            `catch` \e ->
                unless (isDoesNotExistError e) $ throwIO e

    changeDocId old new = do
        renamed <- Storage $ do
            Handle{hDataDir} <- ask
            let oldPath = hDataDir </> docDir old
                newPath = hDataDir </> docDir new
            oldPathCanon <- liftIO $ canonicalizePath oldPath
            newPathCanon <- liftIO $ canonicalizePath newPath
            let pathsDiffer = newPathCanon /= oldPathCanon
            when pathsDiffer $ do
                newPathExists <- liftIO $ doesPathExist newPath
                when newPathExists $
                    throwErrorString $ unwords
                        [ "changeDocId"
                        , show old, "[", oldPath, "->", oldPathCanon, "]"
                        , show new, "[", newPath, "->", newPathCanon, "]"
                        , ": internal error: new document id is already taken"
                        ]
                liftIO $ renameDirectory oldPath newPath
            pure pathsDiffer
        when renamed $ emitDocumentChanged new

-- | Storage handle (uses the “Handle pattern”).
data Handle = Handle
    { hClock             :: IORef EpochTime
    , hDataDir           :: FilePath
    , hReplica           :: ReplicaId
    , hOnDocumentChanged :: TChan CollectionDocId
    }

emitDocumentChanged :: Collection a => DocId a -> Storage ()
emitDocumentChanged docid = Storage $ do
    Handle{hOnDocumentChanged} <- ask
    liftIO . atomically $ writeTChan hOnDocumentChanged $ CollectionDocId docid

-- | Create new storage handle
newHandle :: FilePath -> IO Handle
newHandle hDataDir = do
    randomId <- fst . random <$> newTFGen
    macAddress <- getMacAddress
    let hReplicaId = fromMaybe randomId macAddress
    newHandleWithReplicaId hDataDir hReplicaId

newHandleWithReplicaId :: FilePath -> Word64 -> IO Handle
newHandleWithReplicaId hDataDir hReplicaId = do
    time <- getCurrentEpochTime
    hClock <- newIORef time
    let hReplica = applicationSpecific hReplicaId
    hOnDocumentChanged <- newBroadcastTChanIO
    pure Handle{hDataDir, hClock, hReplica, hOnDocumentChanged}

listDirectoryIfExists :: FilePath -> Storage [FilePath]
listDirectoryIfExists relpath = Storage $ do
    Handle{hDataDir} <- ask
    let dir = hDataDir </> relpath
    liftIO $ do
        exists <- doesDirectoryExist dir
        if exists then listDirectory dir else pure []

docDir :: forall a . Collection a => DocId a -> FilePath
docDir (DocId dir) = collectionName @a </> dir

-- MAC address

getMacAddress :: IO (Maybe Word64)
getMacAddress = do
    macAddress <- getMac
    pure $ decodeMac <$> macAddress
  where
    getMac
        =   listToMaybe
        .   filter (/= minBound)
        .   map mac
        <$> getNetworkInterfaces
    decodeMac (MAC b5 b4 b3 b2 b1 b0)
        = fromIntegral b5 `shiftL` 40
        + fromIntegral b4 `shiftL` 32
        + fromIntegral b3 `shiftL` 24
        + fromIntegral b2 `shiftL` 16
        + fromIntegral b1 `shiftL` 8
        + fromIntegral b0

subscribeForever :: Handle -> (CollectionDocId -> IO ()) -> IO ()
subscribeForever Handle{hOnDocumentChanged} action = do
    childChan <- atomically $ dupTChan hOnDocumentChanged
    forever $ do
        docId <- atomically $ readTChan childChan
        action docId
