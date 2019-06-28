{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Data.Internal (
    ReducedChunk (..),
    Reducer (..),
    Reducible (..),
    Replicated (..),
    ReplicatedAsObject (..),
    ReplicatedAsPayload (..),
    Unapplied,
    WireReducer,
    eqPayload,
    eqRef,
    getObjectStateChunk,
    mkStateChunk,
    newObjectState,
    --
    objectEncoding,
    payloadEncoding,
    --
    fromRon,
    newRon,
) where

import           RON.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           RON.Error (MonadE, errorContext, liftMaybe)
import           RON.Event (ReplicaClock)
import           RON.Types (Atom (AInteger, AString, AUuid), Object (Object),
                            ObjectState (ObjectState, frame, id), Op (Op, opId),
                            StateChunk (StateChunk, stateBody, stateType, stateVersion),
                            StateFrame, UUID (UUID), WireChunk)
import           RON.UUID (zero)

-- | Reduce all chunks of specific type and object in the frame
type WireReducer = UUID -> NonEmpty WireChunk -> [WireChunk]

data Reducer = Reducer
    { wireReducer  :: WireReducer
    , stateReducer :: StateChunk -> StateChunk -> StateChunk
    }

-- | Unapplied patches and raw ops
type Unapplied = ([ReducedChunk], [Op])

-- TODO(2018-08-24, cblp, #26) Semilattice a?
-- | Untyped-reducible types.
-- Untyped means if this type is a container then the types of data contained in
-- it is not considered.
class (Eq a, Monoid a) => Reducible a where

    -- | UUID of the type
    reducibleOpType :: UUID

    -- | Load a state from a state chunk
    stateFromChunk :: [Op] -> a

    -- | Store a state to a state chunk
    stateToChunk :: a -> StateChunk

    -- | Merge a state with patches and raw ops
    applyPatches :: a -> Unapplied -> (a, Unapplied)
    applyPatches a (patches, ops) =
        ( a <> foldMap (patchValue . patchFromChunk) patches
            <> foldMap (patchValue . patchFromRawOp) ops
        , mempty
        )

    -- | Merge patches and raw ops into bigger patches or throw obsolete ops
    reduceUnappliedPatches :: Unapplied -> Unapplied
    reduceUnappliedPatches (patches, ops) =
        ( maybeToList .
            fmap (patchToChunk @a . sconcat) .
            nonEmpty $
            map patchFromChunk patches <> map patchFromRawOp ops
        , []
        )

data ReducedChunk = ReducedChunk
    { rcVersion :: UUID
    , rcRef     :: UUID
    , rcBody    :: [Op]
    }
    deriving (Show)

mkChunkVersion :: [Op] -> UUID
mkChunkVersion = maximumDef zero . map opId

mkStateChunk :: UUID -> [Op] -> StateChunk
mkStateChunk stateType ops =
    StateChunk{stateType, stateVersion = mkChunkVersion ops, stateBody = ops}

data Patch a = Patch{patchRef :: UUID, patchValue :: a}

instance Semigroup a => Semigroup (Patch a) where
    Patch ref1 a1 <> Patch ref2 a2 = Patch (min ref1 ref2) (a1 <> a2)

patchFromRawOp :: Reducible a => Op -> Patch a
patchFromRawOp op@Op{opId} = Patch
    { patchRef = opId
    , patchValue = stateFromChunk [op]
    }

patchFromChunk :: Reducible a => ReducedChunk -> Patch a
patchFromChunk ReducedChunk{..} =
    Patch{patchRef = rcRef, patchValue = stateFromChunk rcBody}

patchToChunk :: Reducible a => Patch a -> ReducedChunk
patchToChunk Patch{patchRef, patchValue} =
    ReducedChunk{rcRef = patchRef, rcVersion = stateVersion, rcBody = stateBody}
  where
    StateChunk{stateVersion, stateBody} = stateToChunk patchValue

-- | Base class for typed encoding
class Replicated a where
    -- | Instances SHOULD implement 'encoding' either as 'objectEncoding' or as
    -- 'payloadEncoding'
    encoding :: Encoding a

data Encoding a = Encoding
    { encodingNewRon
        :: forall m
        . (ReplicaClock m, MonadState StateFrame m) => a -> m [Atom]
    , encodingFromRon
        :: forall m . (MonadE m, MonadState StateFrame m) => [Atom] -> m a
    }

-- | Encode typed data to a payload with possible addition objects
newRon
    :: (Replicated a, ReplicaClock m, MonadState StateFrame m) => a -> m [Atom]
newRon = encodingNewRon encoding

-- | Decode typed data from a payload.
-- The implementation may use other objects in the frame to resolve references.
-- TODO(2019-06-28, cblp) use 'ReaderT' for symmetry with 'newRon'
fromRon :: (MonadE m, Replicated a, MonadState StateFrame m) => [Atom] -> m a
fromRon = encodingFromRon encoding

-- | Standard implementation of 'Replicated' for 'ReplicatedAsObject' types.
objectEncoding :: ReplicatedAsObject a => Encoding a
objectEncoding = Encoding
    { encodingNewRon = \a -> do
        Object oid <- newObject a
        pure [AUuid oid]
    , encodingFromRon = objectFromRon getObject
    }

-- | Standard implementation of 'Replicated' for 'ReplicatedAsPayload' types.
payloadEncoding :: ReplicatedAsPayload a => Encoding a
payloadEncoding = Encoding
    { encodingNewRon  = pure . toPayload
    , encodingFromRon = \atoms -> fromPayload atoms
    }

-- | Instances of this class are encoded as payload only.
class Replicated a => ReplicatedAsPayload a where

    -- | Encode data
    toPayload :: a -> [Atom]

    -- | Decode data
    fromPayload :: MonadE m => [Atom] -> m a

instance Replicated Int64 where encoding = payloadEncoding

instance ReplicatedAsPayload Int64 where
    toPayload int = [AInteger int]
    fromPayload atoms = errorContext "Integer" $ case atoms of
        [AInteger int] -> pure int
        _              -> throwError "Expected Integer syntax"

instance Replicated UUID where encoding = payloadEncoding

instance ReplicatedAsPayload UUID where
    toPayload u = [AUuid u]
    fromPayload atoms = errorContext "UUID" $ case atoms of
        [AUuid u] -> pure u
        _         -> throwError "Expected UUID syntax"

instance Replicated Text where encoding = payloadEncoding

instance ReplicatedAsPayload Text where
    toPayload t = [AString t]
    fromPayload atoms = errorContext "String" $ case atoms of
        [AString t] -> pure t
        _           -> throwError "Expected string syntax"

instance Replicated Char where encoding = payloadEncoding

instance ReplicatedAsPayload Char where
    toPayload c = [AString $ Text.singleton c]
    fromPayload atoms = errorContext "Char" $ case atoms of
        [AString (Text.uncons -> Just (c, ""))] -> pure c
        _ -> throwError "Expected one-character string"

-- | Instances of this class are encoded as objects.
-- An enclosing object's payload will be filled with this object's id.
class Replicated a => ReplicatedAsObject a where

    -- | UUID of the type
    objectOpType :: UUID

    -- | Encode data. Write frame and return id.
    newObject :: (ReplicaClock m, MonadState StateFrame m) => a -> m (Object a)

    -- | Decode data
    getObject :: (MonadE m, MonadState StateFrame m) => Object a -> m a

objectFromRon :: MonadE m => (Object a -> m a) -> [Atom] -> m a
objectFromRon handler atoms = case atoms of
    [AUuid oid] -> handler $ Object oid
    _           -> throwError "Expected object UUID"

-- | Create new 'ObjectState' from a value
newObjectState
    :: (ReplicatedAsObject a, ReplicaClock m) => a -> m (ObjectState a)
newObjectState a = do
    (Object id, frame) <- runStateT (newObject a) mempty
    pure $ ObjectState{id, frame}

getObjectStateChunk
    :: (MonadE m, MonadState StateFrame m) => Object a -> m StateChunk
getObjectStateChunk (Object oid) = do
    frame <- get
    liftMaybe "no such object in chunk" $ Map.lookup oid frame

eqRef :: Object a -> [Atom] -> Bool
eqRef (Object oid) atoms = case atoms of
    [AUuid ref] -> oid == ref
    _           -> False

eqPayload :: ReplicatedAsPayload a => a -> [Atom] -> Bool
eqPayload a atoms = toPayload a == atoms

pattern None :: Atom
pattern None = AUuid (UUID 0xcb3ca9000000000 0)  -- none

pattern Some :: Atom
pattern Some = AUuid (UUID 0xdf3c69000000000 0)  -- some

instance Replicated a => Replicated (Maybe a) where
    encoding = Encoding
        { encodingNewRon = \case
            Just a  -> (Some :) <$> newRon a
            Nothing -> pure [None]
        , encodingFromRon = \atoms ->
            errorContext "Option" $ case atoms of
                Some : atoms' -> Just <$> fromRon atoms'
                [None]        -> pure Nothing
                _             -> throwError "Bad Option"
        }

instance ReplicatedAsPayload a => ReplicatedAsPayload (Maybe a) where
    toPayload = \case
        Just a  -> Some : toPayload a
        Nothing -> [None]
    fromPayload = errorContext "Option" . \case
        Some : atoms -> Just <$> fromPayload atoms
        [None]       -> pure Nothing
        _            -> throwError "Bad Option"

pattern ATrue :: Atom
pattern ATrue = AUuid (UUID 0xe36e69000000000 0)  -- true

pattern AFalse :: Atom
pattern AFalse = AUuid (UUID 0xaa5c37a40000000 0)  -- false

instance Replicated Bool where encoding = payloadEncoding

instance ReplicatedAsPayload Bool where
    toPayload b
        | b         = [ATrue]
        | otherwise = [AFalse]

    fromPayload = errorContext "Bool" . \case
        [ATrue]  -> pure True
        [AFalse] -> pure False
        _        -> throwError "Expected single UUID `true` or `false`"
