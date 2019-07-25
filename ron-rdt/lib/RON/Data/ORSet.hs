{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Observed-Remove Set (OR-Set)
module RON.Data.ORSet
    ( ORSet (..)
    , ORSetItem (..)
    , ORSetMap
    , ORSetRaw
    , addRef
    , addValue
    , findAnyAlive
    , findAnyAlive'
    , removeRef
    , removeValue
    , zoom
    ) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (MonadObjectState, ObjectStateT, Reducible,
                                    Replicated, ReplicatedAsObject,
                                    ReplicatedAsPayload, encoding, eqPayload,
                                    eqRef, fromRon, getObject,
                                    getObjectStateChunk, mkStateChunk,
                                    modifyObjectStateChunk_, newObject, newRon,
                                    objectEncoding, objectOpType,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk)
import           RON.Error (MonadE, throwErrorText)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Types (Atom (AUuid), Object (Object),
                            Op (Op, opId, payload, refId),
                            StateChunk (StateChunk, stateBody, stateType),
                            UUID)
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
    deriving (Eq, Show)

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

    getObject = do
        StateChunk{stateBody} <- getObjectStateChunk
        mItems <- for stateBody $ \Op{refId, payload} -> case refId of
            Zero -> do
                item <- fromRon payload
                pure $ Just item
            _    -> pure Nothing
        pure . ORSet $ catMaybes mItems

-- | XXX Internal. Common implementation of 'addValue' and 'addRef'.
commonAdd :: (MonadE m, MonadObjectState a m, ReplicaClock m) => [Atom] -> m ()
commonAdd payload =
    modifyObjectStateChunk_ $ \StateChunk{stateBody} -> do
        event <- getEventUuid
        let newOp = Op event Zero payload
        let chunk' = stateBody ++ [newOp]
        pure StateChunk
            {stateType = setType, stateVersion = event, stateBody = chunk'}

-- | Encode a value and add a it to the OR-Set
addValue
    :: (Replicated a, ReplicaClock m, MonadE m, MonadObjectState (ORSet a) m)
    => a -> m ()
addValue item = do
    payload <- newRon item
    commonAdd payload

addRef
    :: (ReplicaClock m, MonadE m, MonadObjectState (ORSet a) m)
    => Object a -> m ()
addRef (Object itemUuid) = commonAdd [AUuid itemUuid]

-- | XXX Internal. Common implementation of 'removeValue' and 'removeRef'.
commonRemove
    :: (MonadE m, ReplicaClock m, MonadObjectState (ORSet a) m)
    => ([Atom] -> Bool) -> m ()
commonRemove isTarget =
    modifyObjectStateChunk_ $ \chunk@StateChunk{stateBody} -> do
        let state0@(ORSetRaw opMap) = stateFromChunk stateBody
        let targetEvents =
                [ opId
                | Op{opId, refId, payload} <- toList opMap
                , refId == Zero  -- is alive
                , isTarget payload
                ]
        case targetEvents of
            [] -> pure chunk
            _  -> do
                tombstone <- getEventUuid
                let patch =
                        [ Op{opId = tombstone, refId, payload = []}
                        | refId <- targetEvents
                        ]
                let state' = state0 <> stateFromChunk patch
                pure $ stateToChunk state'

-- | Remove an atomic value from the OR-Set
removeValue
    ::  ( ReplicatedAsPayload a
        , MonadE m, ReplicaClock m, MonadObjectState (ORSet a) m
        )
    => a -> m ()
removeValue = commonRemove . eqPayload

-- | Remove an object reference from the OR-Set
removeRef
    :: (MonadE m, ReplicaClock m, MonadObjectState (ORSet a) m)
    => Object a -> m ()
removeRef = commonRemove . eqRef

-- | Reference to an item inside an 'ORSet'.
newtype ORSetItem a = ORSetItem UUID
    deriving (Show)

-- | Go from modification of the whole set to the modification of an item
-- object.
zoom
    :: MonadE m
    => ORSetItem item -> ObjectStateT item m a -> ObjectStateT (ORSet item) m a
zoom (ORSetItem key) innerModifier = do
    StateChunk{stateBody} <- getObjectStateChunk
    let ORSetRaw opMap = stateFromChunk stateBody
    itemValueRef <- case Map.lookup key opMap of
        Nothing ->
            -- TODO(2019-08-07, cblp) creat empty object?
            throwErrorText "no such key in ORSet"
        Just Op{payload} -> case payload of
            [AUuid itemValueRef] -> pure itemValueRef
            _ -> throwErrorText "item payload is not an object ref"
    lift $ runReaderT innerModifier $ Object itemValueRef

-- | Find any alive item. If no alive item found, return 'Nothing'.
findAnyAlive
    :: (MonadE m, MonadObjectState (ORSet item) m) => m (Maybe (ORSetItem item))
findAnyAlive = do
    StateChunk{stateBody} <- getObjectStateChunk
    pure $ let
        ORSetRaw opMap = stateFromChunk stateBody
        aliveItems = [op | op@Op{refId = UUID.Zero} <- toList opMap]
        in
        case listToMaybe aliveItems of
            Nothing       -> Nothing
            Just Op{opId} -> Just $ ORSetItem opId

-- | Find any alive item. If no alive item found, report an error.
findAnyAlive'
    :: (MonadE m, MonadObjectState (ORSet item) m) => m (ORSetItem item)
findAnyAlive' = do
    mx <- findAnyAlive
    case mx of
        Just x  -> pure x
        Nothing -> throwErrorText "empty set"

type ORSetMap k v = ORSet (k, v)
