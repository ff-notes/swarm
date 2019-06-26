{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Data.Internal (
    Encoding (..),
    ReducedChunk (..),
    Reducer (..),
    Reducible (..),
    Replicated (..),
    ReplicatedAsObject (..),
    ReplicatedAsPayload (..),
    ReplicatedBoundedSemilattice (..),
    Unapplied,
    WireReducer,
    advanceToObject,
    eqPayload,
    eqRef,
    getObjectStateChunk,
    modifyObjectStateChunk,
    modifyObjectStateChunk_,
    newObjectFrame,
    reduceState,
    reduceObjectStates,
    --
    objectEncoding,
    objectRconcat,
    payloadEncoding,
    --
    fromRon,
    newRon,
    --
    ObjectStateT,
    MonadObjectState,
) where

import           RON.Prelude

import           Data.Foldable (foldl1)
import           Data.List (minimum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           RON.Error (MonadE, errorContext, liftMaybe)
import           RON.Event (ReplicaClock, advanceToUuid)
import           RON.Semilattice (BoundedSemilattice)
import           RON.Types (Atom (AInteger, AString, AUuid), Object (Object),
                            ObjectFrame (ObjectFrame, frame, uuid),
                            Op (Op, opId, payload, refId), Payload,
                            StateChunk (StateChunk, stateBody), StateFrame,
                            UUID (UUID), WireChunk)

-- | Reduce all chunks of specific type and object in the frame
type WireReducer = UUID -> NonEmpty WireChunk -> [WireChunk]

data Reducer = Reducer
    { wireReducer  :: WireReducer
    , stateReducer :: StateChunk -> StateChunk -> StateChunk
    }

-- | Unapplied patches and raw ops
type Unapplied = ([ReducedChunk], [Op])

-- | Untyped-reducible types.
-- Untyped means if this type is a container then the types of data contained in
-- it is not considered.
class (Eq a, BoundedSemilattice a) => Reducible a where

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
    { rcRef     :: UUID
    , rcBody    :: [Op]
    }
    deriving (Show)

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
    ReducedChunk{rcRef = patchRef, rcBody = stateBody}
  where
    StateChunk{stateBody} = stateToChunk patchValue

-- | Base class for typed encoding
class Replicated a where
    -- | Instances SHOULD implement 'encoding' either as 'objectEncoding' or as
    -- 'payloadEncoding'
    encoding :: Encoding a

data Encoding a = Encoding
    { encodingNewRon
        :: forall m
        . (ReplicaClock m, MonadState StateFrame m) => a -> m Payload
    , encodingFromRon
        :: forall m . (MonadE m, MonadState StateFrame m) => Payload -> m a
    }

-- | Encode typed data to a payload with possible addition objects
newRon
    :: (Replicated a, ReplicaClock m, MonadState StateFrame m) => a -> m Payload
newRon = encodingNewRon encoding

-- | Decode typed data from a payload.
-- The implementation may use other objects in the frame to resolve references.
fromRon :: (MonadE m, Replicated a, MonadState StateFrame m) => Payload -> m a
fromRon = encodingFromRon encoding

-- | Standard implementation of 'Replicated' for 'ReplicatedAsObject' types.
objectEncoding :: ReplicatedAsObject a => Encoding a
objectEncoding = Encoding
    { encodingNewRon = \a -> do
        Object uuid <- newObject a
        pure [AUuid uuid]
    , encodingFromRon = objectFromRon $ runReaderT getObject
    }

-- | Standard implementation of 'Replicated' for 'ReplicatedAsPayload' types.
payloadEncoding :: ReplicatedAsPayload a => Encoding a
payloadEncoding = Encoding
    { encodingNewRon  = pure . toPayload
    , encodingFromRon = fromPayload
    }

-- | Instances of this class are encoded as payload only.
--
-- Law: @'encoding' == 'payloadEncoding'@
class Replicated a => ReplicatedAsPayload a where

    -- | Encode data
    toPayload :: a -> Payload

    -- | Decode data
    fromPayload :: MonadE m => Payload -> m a

instance Replicated Int64 where encoding = payloadEncoding

instance ReplicatedAsPayload Int64 where
    toPayload int = [AInteger int]
    fromPayload atoms = errorContext "Integer" $ case atoms of
        [AInteger int] -> pure int
        _              -> throwError "Expected Integer"

instance Replicated UUID where encoding = payloadEncoding

instance ReplicatedAsPayload UUID where
    toPayload u = [AUuid u]
    fromPayload atoms = errorContext "UUID" $ case atoms of
        [AUuid u] -> pure u
        _         -> throwError "Expected UUID"

instance Replicated Text where encoding = payloadEncoding

instance ReplicatedAsPayload Text where
    toPayload t = [AString t]
    fromPayload atoms = errorContext "String" $ case atoms of
        [AString t] -> pure t
        _           -> throwError "Expected String"

instance Replicated Char where encoding = payloadEncoding

instance ReplicatedAsPayload Char where
    toPayload c = [AString $ Text.singleton c]
    fromPayload atoms = errorContext "Char" $ case atoms of
        [AString (Text.uncons -> Just (c, ""))] -> pure c
        _ -> throwError "Expected one-character string"

-- | Instances of this class are encoded as objects.
-- An enclosing object's payload will be filled with this object's id.
--
-- Law: @'encoding' == 'objectEncoding'@
class (Reducible (Rep a), ReplicatedBoundedSemilattice a) =>
    ReplicatedAsObject a where

    -- | Untyped RON-RDT representation
    type Rep a

    -- | Encode data. Write frame and return id.
    newObject :: (ReplicaClock m, MonadState StateFrame m) => a -> m (Object a)

    -- | Decode data
    getObject :: (MonadE m, MonadObjectState a m) => m a

objectFromRon :: MonadE m => (Object a -> m a) -> Payload -> m a
objectFromRon handler atoms = case atoms of
    [AUuid uuid] -> handler $ Object uuid
    _            -> throwError "Expected object UUID"

-- | Create new 'ObjectFrame' from a value
newObjectFrame
    :: (ReplicatedAsObject a, ReplicaClock m) => a -> m (ObjectFrame a)
newObjectFrame a = do
    (Object uuid, frame) <- runStateT (newObject a) mempty
    pure $ ObjectFrame{uuid, frame}

getObjectStateChunk :: (MonadE m, MonadObjectState a m) => m StateChunk
getObjectStateChunk = do
    Object uuid <- ask
    frame <- get
    liftMaybe "no such object in chunk" $ Map.lookup uuid frame

modifyObjectStateChunk
    :: (MonadObjectState a m, ReplicaClock m, MonadE m)
    => (StateChunk -> m (b, StateChunk)) -> m b
modifyObjectStateChunk f = do
    advanceToObject
    Object uuid <- ask
    chunk <- getObjectStateChunk
    (a, chunk') <- f chunk
    modify' $ Map.insert uuid chunk'
    pure a

modifyObjectStateChunk_
    :: (MonadObjectState a m, ReplicaClock m, MonadE m)
    => (StateChunk -> m StateChunk) -> m ()
modifyObjectStateChunk_ f = modifyObjectStateChunk $ \chunk -> do
    chunk' <- f chunk
    pure ((), chunk')

eqRef :: Object a -> Payload -> Bool
eqRef (Object uuid) atoms = case atoms of
    [AUuid ref] -> uuid == ref
    _           -> False

eqPayload :: ReplicatedAsPayload a => a -> Payload -> Bool
eqPayload a atoms = toPayload a == atoms

pattern Some :: Atom
pattern Some = AUuid (UUID 0xdf3c69000000000 0)  -- some

instance Replicated a => Replicated (Maybe a) where
    encoding = Encoding
        { encodingNewRon = \case
            Just a  -> (Some :) <$> newRon a
            Nothing -> pure []
        , encodingFromRon = \atoms ->
            errorContext "Option" $ case atoms of
                Some : atoms' -> Just <$> fromRon atoms'
                _             -> pure Nothing
        }

instance ReplicatedAsPayload a => ReplicatedAsPayload (Maybe a) where
    toPayload = \case
        Just a  -> Some : toPayload a
        Nothing -> []
    fromPayload = errorContext "Option" . \case
        Some : atoms -> Just <$> fromPayload atoms
        _            -> pure Nothing

instance ReplicatedBoundedSemilattice a =>
    ReplicatedBoundedSemilattice (Maybe a) where

    rconcat payloads = case payloads' of
        []   -> pure Nothing
        p:ps -> Just <$> rconcat (p :| ps)
      where
        payloads' = [targetPayload | Some : targetPayload <- toList payloads]

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

type ObjectStateT b m a = ReaderT (Object b) (StateT StateFrame m) a

type MonadObjectState b m = (MonadReader (Object b) m, MonadState StateFrame m)

advanceToObject :: (MonadE m, MonadObjectState a m, ReplicaClock m) => m ()
advanceToObject = do
    Object uuid <- ask
    StateChunk{stateBody} <- getObjectStateChunk
    advanceToUuid $
        maximumDef
            uuid
            [ max opId $ maximumDef refId $ mapMaybe atomAsUuid payload
            | Op{opId, refId, payload} <- stateBody
            ]
  where
    -- | TODO(2019-07-26, cblp) Use lens
    atomAsUuid = \case
        AUuid u -> Just u
        _       -> Nothing

class Replicated a => ReplicatedBoundedSemilattice a where
    rconcat :: (MonadE m, MonadState StateFrame m) => NonEmpty Payload -> m a

reduceState :: forall a . Reducible a => StateChunk -> StateChunk -> StateChunk
reduceState s1 s2 =
    stateToChunk @a $ ((<>) `on` (stateFromChunk . stateBody)) s1 s2

reduceObjectStates
    :: forall a m
    . (MonadE m, MonadState StateFrame m, ReplicatedAsObject a)
    => NonEmpty (Object a) -> m (Object a)
reduceObjectStates (obj :| objs) = do
    chunks <- for (obj :| objs) $ runReaderT getObjectStateChunk
    let chunk = foldl1 (reduceState @(Rep a)) chunks
    let oid = minimum [i | Object i <- obj:objs]
    modify' $ Map.insert oid chunk
    pure $ Object oid

-- | Implementation of 'rconcat' for 'ReplicatedAsObject'
objectRconcat
    :: forall a m
    . (MonadE m, MonadState StateFrame m, ReplicatedAsObject a)
    => NonEmpty Payload -> m a
objectRconcat payloads = do
    states <- for payloads $ fmap Object . fromPayload
    Object ref <- reduceObjectStates @a states
    fromRon [AUuid ref]
