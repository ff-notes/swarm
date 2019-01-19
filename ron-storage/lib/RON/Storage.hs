{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | RON File Storage. For usage, see "RON.Storage.IO".
module RON.Storage (
    Collection (..),
    CollectionName,
    DocId (..),
    Document (..),
    DocVersion,
    MonadStorage (..),
    createDocument,
    decodeDocId,
    docIdFromUuid,
    loadDocument,
    modify,
    readVersion,
) where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String (fromString)
import qualified Data.Text as Text

import           RON.Data (ReplicatedAsObject, reduceObject)
import           RON.Error (Error (Error), MonadE, errorContext, liftMaybe,
                            throwErrorString)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (Object (Object), UUID, objectFrame, objectId)
import           RON.Util (ByteStringL)
import qualified RON.UUID as UUID

-- | Document version identifier (file name)
type DocVersion = FilePath

-- | Document identifier (directory name),
-- should be a RON-Base32-encoded RON-UUID.
newtype DocId a = DocId FilePath
    deriving (Eq, Ord)

instance Collection a => Show (DocId a) where
    show (DocId file) = collectionName @a </> file

-- | Collection (directory name)
type CollectionName = FilePath

-- | A type that intended to be put in a separate collection must define a
-- Collection instance.
class (ReplicatedAsObject a, Typeable a) => Collection a where

    collectionName :: CollectionName

    -- | Called when RON parser fails.
    fallbackParse :: MonadE m => UUID -> ByteStringL -> m (Object a)
    fallbackParse _ _ = throwError "no fallback parser implemented"

-- | Storage backend interface
class (ReplicaClock m, MonadE m) => MonadStorage m where
    getCollections :: m [CollectionName]

    -- | Must return @[]@ for non-existent collection
    getDocuments :: Collection a => m [DocId a]

    -- | Must return @[]@ for non-existent document
    getDocumentVersions :: Collection a => DocId a -> m [DocVersion]

    -- | Must create collection and document if not exist
    saveVersionContent
        :: Collection a => DocId a -> DocVersion -> ByteStringL -> m ()

    loadVersionContent :: Collection a => DocId a -> DocVersion -> m ByteStringL

    deleteVersion :: Collection a => DocId a -> DocVersion -> m ()

    changeDocId :: Collection a => DocId a -> DocId a -> m ()

-- | Try decode UUID from a file name
decodeDocId
    :: DocId a
    -> Maybe (Bool, UUID)  -- ^ Bool = is document id a valid UUID encoding
decodeDocId (DocId file) = do
    uuid <- UUID.decodeBase32 file
    pure (UUID.encodeBase32 uuid == file, uuid)

-- | Load document version as an object
readVersion
    :: MonadStorage m
    => Collection a => DocId a -> DocVersion -> m (Object a, IsTouched)
readVersion docid version = do
    (isObjectIdValid, objectId) <-
        liftMaybe (Text.pack $ "Bad Base32 UUID " ++ show docid) $
        decodeDocId docid
    unless isObjectIdValid $
        throwErrorString $ "Not a Base32 UUID " ++ show docid
    contents <- loadVersionContent docid version
    case parseStateFrame contents of
        Right objectFrame ->
            pure (Object{objectId, objectFrame}, IsTouched False)
        Left ronError ->
            do  object <- fallbackParse objectId contents
                pure (object, IsTouched True)
            `catchError` \fallbackError ->
                throwError $ case BSLC.head contents of
                    '{' -> fallbackError
                    _   -> fromString ronError

-- | A thing (e.g. document) was fixed during loading.
-- It it was fixed during loading it must be saved to the storage.
newtype IsTouched = IsTouched Bool
    deriving Show

-- | Result of DB reading, loaded document with information about its versions
data Document a = Document
    { value     :: Object a
        -- ^ Merged value.
    , versions  :: NonEmpty DocVersion
    , isTouched :: IsTouched
    }
    deriving Show

-- | Load all versions of a document
loadDocument :: (Collection a, MonadStorage m) => DocId a -> m (Document a)
loadDocument docid = loadRetry (3 :: Int)
  where
    loadRetry n
        | n > 0 = do
            versions0 <- getDocumentVersions docid
            case versions0 of
                []   ->
                    throwErrorString $
                    "Document with id " ++ show docid ++ " has not found."
                v:vs -> do
                    let versions = v :| vs
                    let wrapDoc (value, isTouched) =
                            Document{value, versions, isTouched}
                    readResults <-
                        errorContext ("document " <> Text.pack (show docid)) $
                        for versions $ \ver ->
                            try $
                            errorContext ("version " <> Text.pack ver) $
                            readVersion docid ver
                    liftEither $ wrapDoc <$> vsconcat readResults
        | otherwise = throwError "Maximum retries exceeded"

-- | Validation-like version of 'sconcat'.
vsconcat
    :: NonEmpty (Either Error (Object a, IsTouched))
    -> Either Error (Object a, IsTouched)
vsconcat = foldr1 vappend
  where
    vappend    (Left  e1)    (Left  e2) = Left $ Error "vappend" [e1, e2]
    vappend e1@(Left  _ )    (Right _ ) = e1
    vappend    (Right _ ) e2@(Left  _ ) = e2
    vappend    (Right r1)    (Right r2) =
        (, IsTouched (t1 || t2)) <$> reduceObject a1 a2 where
        (a1, IsTouched t1) = r1
        (a2, IsTouched t2) = r2

try :: MonadError e m => m a -> m (Either e a)
try ma = (Right <$> ma) `catchError` (pure . Left)

-- | Load document, apply changes and put it back to storage
modify
    :: (Collection a, MonadStorage m)
    => DocId a -> StateT (Object a) m () -> m (Object a)
modify docid f = do
    oldDoc <- loadDocument docid
    newObj <- execStateT f $ value oldDoc
    createVersion (Just (docid, oldDoc)) newObj
    pure newObj

-- | Create new version of an object/document.
-- If the document doesn't exist yet, it will be created.
createVersion
    :: forall a m
    . (Collection a, MonadStorage m)
    => Maybe (DocId a, Document a)
        -- ^ 'Just', if document exists already; 'Nothing' otherwise.
    -> Object a
    -> m ()
createVersion mDoc newObj = case mDoc of
    Nothing -> save (DocId @a $ UUID.encodeBase32 objectId) []
    Just (docid, oldDoc) -> do
        let Document{value = oldObj, versions, isTouched = IsTouched isTouched}
                = oldDoc
        when (newObj /= oldObj || length versions /= 1 || isTouched) $
            save docid versions
  where
    Object{objectId, objectFrame} = newObj

    save docid oldVersions = do
        newVersion <- UUID.encodeBase32 <$> getEventUuid
        saveVersionContent docid newVersion (serializeStateFrame objectFrame)
        for_ oldVersions $ deleteVersion docid

-- | Create document assuming it doesn't exist yet.
createDocument :: (Collection a, MonadStorage m) => Object a -> m ()
createDocument = createVersion Nothing

docIdFromUuid :: UUID -> DocId a
docIdFromUuid = DocId . UUID.encodeBase32
