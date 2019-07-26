{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Observed-Remove Set (OR-Set)
module RON.Data.ORSet (
    ORSet (..),
    ORSetItem (..),
    ORSetMap,
    ORSetRep,
    addRef,
    addValue,
    findAnyAlive,
    findAnyAlive',
    removeRef,
    removeValue,
    zoomItem,
    -- * struct_set
    assignField,
    newObject,
    readField,
    viewField,
    viewFieldFolded,
    viewFieldLww,
    zoomField,
) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (MonadObjectState, ObjectStateT, Reducible,
                                    Replicated, ReplicatedAsObject,
                                    ReplicatedAsPayload,
                                    ReplicatedBoundedSemilattice, encoding,
                                    eqPayload, eqRef, fromRon, getObject,
                                    getObjectStateChunk, mkStateChunk,
                                    modifyObjectStateChunk_, newRon,
                                    objectEncoding, objectRconcat, rconcat,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk)
import qualified RON.Data.Internal
import           RON.Error (MonadE, errorContext, throwErrorText)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Semilattice (Semilattice)
import           RON.Types (Atom (AUuid), Object (Object),
                            Op (Op, opId, payload, refId),
                            StateChunk (StateChunk, stateBody, stateType, stateVersion),
                            StateFrame, UUID)
import           RON.Util (Instance (Instance))
import           RON.UUID (pattern Zero)
import qualified RON.UUID as UUID

-- | Untyped OR-Set.
-- Implementation:
-- a map from the last change (creation or deletion) to the original op.
newtype ORSetRep = ORSetRep (Map UUID Op)
    deriving (Eq, Show)

opKey :: Op -> UUID
opKey Op{opId, refId} = case refId of
    Zero -> opId   -- alive
    _    -> refId  -- tombstone

observedRemove :: Op -> Op -> Op
observedRemove = maxOn refId

instance Semigroup ORSetRep where
    ORSetRep set1 <> ORSetRep set2 =
        ORSetRep $ Map.unionWith observedRemove set1 set2

instance Monoid ORSetRep where
    mempty = ORSetRep mempty

instance Semilattice ORSetRep

instance Reducible ORSetRep where
    reducibleOpType = setType

    stateFromChunk ops =
        ORSetRep $ Map.fromListWith observedRemove [(opKey op, op) | op <- ops]

    stateToChunk (ORSetRep set) =
        mkStateChunk setType . sortOn opId $ Map.elems set

-- | Name-UUID to use as OR-Set type marker.
setType :: UUID
setType = $(UUID.liftName "set")

-- | Type-directing wrapper for typed OR-Set.
-- 'Eq' instance is purely technical, it doesn't use 'Ord', nor 'Hashable',
-- so its result may be confusing.
newtype ORSet a = ORSet [a]
    deriving (Eq, Show)
{- TODO(2019-07-24, cblp) data family ORSet c a where
    newtype instance ORSet Ord      a = ORSetOrd  (Set     a)
    newtype instance ORSet Hashable a = ORSetHash (HashSet a)
-}

instance Replicated a => Replicated (ORSet a) where
    encoding = objectEncoding

instance Replicated a => ReplicatedBoundedSemilattice (ORSet a) where
    rconcat = objectRconcat

instance Replicated a => ReplicatedAsObject (ORSet a) where
    type Rep (ORSet a) = ORSetRep

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
        let state0@(ORSetRep opMap) = stateFromChunk stateBody
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
zoomItem
    :: MonadE m
    => ORSetItem item -> ObjectStateT item m a -> ObjectStateT (ORSet item) m a
zoomItem (ORSetItem key) innerModifier = do
    StateChunk{stateBody} <- getObjectStateChunk
    let ORSetRep opMap = stateFromChunk stateBody
    itemValueRef <- case Map.lookup key opMap of
        Nothing ->
            -- TODO(2019-07-08, cblp) creat empty object?
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
        ORSetRep opMap = stateFromChunk stateBody
        aliveItems = [op | op@Op{refId = Zero} <- toList opMap]
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

-- | Assign a value to a field
assignField
    :: (Replicated field, ReplicaClock m, MonadE m, MonadObjectState struct m)
    => UUID   -- ^ Field name
    -> field  -- ^ Value
    -> m ()
assignField field value =
    modifyObjectStateChunk_ $ \StateChunk{stateBody} -> do
        event <- getEventUuid
        valuePayload <- newRon value
        let addOp = Op
                { opId = event
                , refId = Zero
                , payload = AUuid field : valuePayload
                }
        let (observed, stateBody1) = partition (isAliveField field) stateBody
        removeOps <- for observed $ \op -> do
            tombstone <- getEventUuid  -- TODO(2019-07-10, cblp) sequential
            pure op{refId = tombstone, payload = [AUuid field]}
        let stateBody2 = sortOn opId $ addOp : removeOps ++ stateBody1
        pure StateChunk
            {stateVersion = event, stateBody = stateBody2, stateType = setType}

isAliveField :: UUID -> Op -> Bool
isAliveField field = \case
    Op{refId = Zero, payload = AUuid field' : _} -> field == field'
    _ -> False

-- | Decode field value
viewField
    ::  ( MonadE m
        , MonadState StateFrame m
        , Replicated a
        )
    =>  UUID        -- ^ Field name
    ->  StateChunk  -- ^ ORSet object chunk
    ->  m [a]
viewField field StateChunk{stateBody}
    = errorContext "ORSet.viewField"
    $ traverse (fromRon . drop 1 . payload)
    $ filter (isAliveField field) stateBody

-- | Decode field value
viewFieldFolded
    ::  ( MonadE m
        , MonadState StateFrame m
        , ReplicatedBoundedSemilattice a
        , ReplicaClock m
        )
    =>  UUID        -- ^ Field name
    ->  StateChunk  -- ^ ORSet object chunk
    ->  m a
viewFieldFolded field StateChunk{stateBody} =
    rconcat $ map (drop 1 . payload) $ filter (isAliveField field) stateBody

-- | Decode field value
viewFieldLww
    :: (MonadE m, MonadState StateFrame m, Replicated a)
    => UUID        -- ^ Field name
    -> StateChunk  -- ^ ORSet object chunk
    -> m a         -- ^ TODO(2019-07-23, cblp) Maybe?
viewFieldLww field StateChunk{stateBody} =
    errorContext "ORSet.viewFieldLww" $ do
        let ops = sortOn (Down . opId) $ filter (isAliveField field) stateBody
        case ops of
            Op{payload = _field : valuePayload} : _ -> fromRon valuePayload
            Op{payload = []} : _ -> error "a field without a field tag found"
            [] -> throwError "no version of LWW field found"

-- | Read field value
readField
    ::  ( MonadE m
        , MonadObjectState struct m
        , Replicated a
        )
    =>  UUID  -- ^ Field name
    ->  m [a]
readField field = do
    stateChunk <- getObjectStateChunk
    viewField field stateChunk

-- | Pseudo-lens to an object inside a specified field
zoomField
    :: MonadE m
    => UUID                     -- ^ Field name
    -> ObjectStateT field  m a  -- ^ Inner object modifier
    -> ObjectStateT struct m a
zoomField field innerModifier =
    errorContext ("ORSet.zoomField" <> show field) $ do
        StateChunk{stateBody} <- getObjectStateChunk
        let ops = filter (isAliveField field) stateBody
        case ops of
            [] ->
                throwErrorText
                    "TODO(2019-07-12, cblp) create object for modifying;\
                    \ use 'reduceStates'"
            [Op{payload}] -> case payload of
                [_field, AUuid fieldObjectId] ->
                    lift $ runReaderT innerModifier $ Object fieldObjectId
                _ -> throwErrorText "TODO(2019-07-12, cblp) skip bad format?"
            _:_:_ ->
                throwErrorText
                    "TODO(2019-07-12, cblp) merge multiple object versions;\
                    \ use 'reduceStates'"

-- | Create an ORSet object from a list of named fields.
newObject
    :: (MonadState StateFrame m, ReplicaClock m)
    => [(UUID, [Instance Replicated])] -> m UUID
newObject fields = do
    stateBody <-
        fmap fold $ for fields $ \(name, values) ->
            for values $ \(Instance value) -> do
                opId <- getEventUuid  -- TODO(2019-07-12, cblp) sequential
                valuePayload <- newRon value
                pure Op{opId, refId = Zero, payload = AUuid name : valuePayload}
    objectId <- getEventUuid
    modify' $
        (<>) $ Map.singleton objectId $
        StateChunk{stateType = setType, stateVersion = objectId, stateBody}
    pure objectId
