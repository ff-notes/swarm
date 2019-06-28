{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | RON Storage details. Use of this module only to implement a backend.
module RON.Storage.Backend (
    Collection (..),
    CollectionName,
    DocId (..),
    Document (..),
    DocVersion,
    IsTouched (..),
    MonadStorage (..),
    createVersion,
    decodeDocId,
    readVersion,
) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String (fromString)
import           System.FilePath ((</>))
import qualified Text.Show (show)

import           RON.Data (ReplicatedAsObject)
import           RON.Error (MonadE, liftMaybe, throwErrorString)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Text (parseStateFrame, serializeStateFrame)
import           RON.Types (ObjectState (ObjectState, frame, id), UUID)
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
    fallbackParse :: MonadE m => UUID -> ByteStringL -> m (ObjectState a)
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
    => Collection a => DocId a -> DocVersion -> m (ObjectState a, IsTouched)
readVersion docid version = do
    (isObjectIdValid, id) <-
        liftMaybe ("Bad Base32 UUID " <> show docid) $
        decodeDocId docid
    unless isObjectIdValid $
        throwErrorString $ "Not a Base32 UUID " ++ show docid
    contents <- loadVersionContent docid version
    case parseStateFrame contents of
        Right frame ->
            pure (ObjectState{id, frame}, IsTouched False)
        Left ronError ->
            do  object <- fallbackParse id contents
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
    { value     :: ObjectState a
        -- ^ Merged value.
    , versions  :: NonEmpty DocVersion
    , isTouched :: IsTouched
    }
    deriving Show

-- | Create new version of an object/document.
-- If the document doesn't exist yet, it will be created.
createVersion
    :: forall a m
    . (Collection a, MonadStorage m)
    => Maybe (DocId a, Document a)
        -- ^ 'Just', if document exists already; 'Nothing' otherwise.
    -> ObjectState a
    -> m ()
createVersion mDoc newObj = case mDoc of
    Nothing -> save (DocId @a $ UUID.encodeBase32 id) []
    Just (docid, oldDoc) -> do
        let Document{value = oldObj, versions, isTouched = IsTouched isTouched}
                = oldDoc
        when (newObj /= oldObj || length versions /= 1 || isTouched) $
            save docid $ toList versions
  where
    ObjectState{id, frame} = newObj

    save docid oldVersions = do
        newVersion <- UUID.encodeBase32 <$> getEventUuid
        saveVersionContent docid newVersion (serializeStateFrame frame)
        for_ oldVersions $ deleteVersion docid
