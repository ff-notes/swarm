{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Observed-Remove Set (OR-Set)
module RON.Data.ORSet
    ( ORSet (..)
    , ORSetRaw
    , addRef
    , addValue
    , removeRef
    , removeValue
    ) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import           RON.Types (Atom (AUuid), Object (Object),
                            Op (Op, opId, payload, refId),
                            StateChunk (StateChunk, stateBody, stateType, stateVersion),
                            StateFrame, UUID)
import           RON.Util (runStateAsWriter)
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

-- | Untyped OR-Set.
-- Implementation:
-- a map from the last change (creation or deletion) to the original op.
newtype ORSetRaw = ORSetRaw (Map UUID Op)
    deriving (Eq, Show)

opKey :: Op -> UUID
opKey Op{opId, refId} = case refId of
    Zero -> opId   -- alive
    _    -> refId  -- tombstone

observedRemove :: Op -> Op -> Op
observedRemove = maxOn refId

instance Semigroup ORSetRaw where
    ORSetRaw set1 <> ORSetRaw set2 =
        ORSetRaw $ Map.unionWith observedRemove set1 set2

instance Monoid ORSetRaw where
    mempty = ORSetRaw mempty

instance Reducible ORSetRaw where
    reducibleOpType = setType

    stateFromChunk ops =
        ORSetRaw $ Map.fromListWith observedRemove [(opKey op, op) | op <- ops]

    stateToChunk (ORSetRaw set) =
        mkStateChunk setType . sortOn opId $ Map.elems set

-- | Name-UUID to use as OR-Set type marker.
setType :: UUID
setType = $(UUID.liftName "set")

-- | Type-directing wrapper for typed OR-Set
newtype ORSet a = ORSet [a]

instance Replicated a => Replicated (ORSet a) where
    encoding = objectEncoding

instance Replicated a => ReplicatedAsObject (ORSet a) where
    objectOpType = setType

    newObject (ORSet items) = do
        ops <- for items $ \item -> do
            event <- getEventUuid
            payload <- newRon item
            pure $ Op event Zero payload
        oid <- getEventUuid
        let stateVersion = maximumDef oid $ map opId ops
        modify' $
            (<>) $ Map.singleton oid $
            StateChunk{stateType = setType, stateVersion, stateBody = ops}
        pure $ Object oid

    getObject obj = do
        StateChunk{stateBody} <- getObjectStateChunk obj
        mItems <- for stateBody $ \Op{refId, payload} -> case refId of
            Zero -> do
                item <- fromRon payload
                pure $ Just item
            _    -> pure Nothing
        pure . ORSet $ catMaybes mItems

-- | XXX Internal. Common part of all modifying operations.
prepareModify
    :: (MonadE m, MonadState (Object a) m, ReplicaClock m) => m (Object a, [Op])
prepareModify = do
    obj <- get
    StateChunk{stateVersion, stateBody} <- getObjectStateChunk obj
    advanceToUuid stateVersion
    pure (obj, stateBody)

-- | XXX Internal. Common implementation of 'addValue' and 'addRef'.
-- commonAdd ::
-- commonAdd =

-- | Encode a value and add a it to the OR-Set
addValue
    :: (Replicated a, ReplicaClock m, MonadE m, MonadState StateFrame m)
    => a -> Object (ORSet a) -> m ()
addValue item self = do
    StateChunk{stateVersion, stateBody} <- getObjectStateChunk self
    advanceToUuid stateVersion
    event <- getEventUuid
    payload <- runStateAsWriter $ newRon item
    let newOp = Op event Zero payload
    let chunk' = stateBody ++ [newOp]
    let state' = StateChunk
            {stateType = setType, stateVersion = event, stateBody = chunk'}
    let Object setUuid = self
    modify' $ Map.insert setUuid state'

addRef
    :: (ReplicaClock m, MonadE m, MonadState StateFrame m)
    => Object a -> Object (ORSet a) -> m ()
addRef (Object itemUuid) self = do
    StateChunk{stateVersion, stateBody} <- getObjectStateChunk self
    advanceToUuid stateVersion
    event <- getEventUuid
    let newOp = Op event Zero [AUuid itemUuid]
    let chunk' = stateBody ++ [newOp]
    let state' = StateChunk
            {stateType = setType, stateVersion = event, stateBody = chunk'}
    let Object setUuid = self
    modify' $ Map.insert setUuid state'

-- | XXX Internal. Common implementation of 'removeValue' and 'removeRef'.
commonRemove
    :: (MonadE m, ReplicaClock m, MonadState StateFrame m)
    => ([Atom] -> Bool) -> Object (ORSet a) -> m ()
commonRemove isTarget self@(Object selfUuid) = do
    StateChunk{stateVersion, stateBody} <- getObjectStateChunk self
    advanceToUuid stateVersion
    let state0@(ORSetRaw opMap) = stateFromChunk stateBody
    let targetEvents =
            [ opId
            | Op{opId, refId, payload} <- toList opMap
            , refId == Zero  -- is alive
            , isTarget payload
            ]
    case targetEvents of
        [] -> pure ()
        _  -> do
            tombstone <- getEventUuid
            let patch =
                    [ Op{opId = tombstone, refId, payload = []}
                    | refId <- targetEvents
                    ]
            let chunk' = state0 <> stateFromChunk patch
            let state' = stateToChunk chunk'
            modify' $ Map.insert selfUuid state'

-- | Remove an atomic value from the OR-Set
removeValue
    ::  ( ReplicatedAsPayload a
        , MonadE m, ReplicaClock m, MonadState StateFrame m
        )
    => a -> Object (ORSet a) -> m ()
removeValue a self = commonRemove (eqPayload a) self

-- | Remove an object reference from the OR-Set
removeRef
    :: (MonadE m, ReplicaClock m, MonadState StateFrame m)
    => Object a -> Object (ORSet a) -> m ()
removeRef = commonRemove . eqRef
