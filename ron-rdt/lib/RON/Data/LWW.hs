{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | LWW-per-field RDT
module RON.Data.LWW
    ( LwwPerField (..)
    , assignField
    , lwwType
    , newObject
    , readField
    , viewField
    , zoomField
    ) where

import           RON.Prelude

import qualified Data.Map.Strict as Map

import           RON.Data.Internal (MonadObjectState, ObjectStateT, Reducible,
                                    Replicated, advanceToObject, fromRon,
                                    getObjectStateChunk, mkStateChunk, newRon,
                                    reducibleOpType, stateFromChunk,
                                    stateToChunk)
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock, getEventUuid)
import           RON.Types (Atom (AUuid), Object (..), Op (..), StateChunk (..),
                            StateFrame, UUID)
import           RON.Util (Instance (Instance))
import qualified RON.UUID as UUID

-- | Last-Write-Wins: select an op with latter event
lww :: Op -> Op -> Op
lww = maxOn opId

-- | Untyped LWW. Implementation: a map from 'opRef' to the original op.
newtype LwwPerField = LwwPerField (Map UUID Op)
    deriving (Eq, Monoid, Show)

instance Semigroup LwwPerField where
    LwwPerField fields1 <> LwwPerField fields2 =
        LwwPerField $ Map.unionWith lww fields1 fields2

instance Reducible LwwPerField where
    reducibleOpType = lwwType

    stateFromChunk ops =
        LwwPerField $ Map.fromListWith lww [(refId, op) | op@Op{refId} <- ops]

    stateToChunk (LwwPerField fields) = mkStateChunk lwwType $ Map.elems fields

-- | Name-UUID to use as LWW type marker.
lwwType :: UUID
lwwType = $(UUID.liftName "lww")

-- | Create LWW object from a list of named fields.
newObject
    :: (MonadState StateFrame m, ReplicaClock m)
    => [(UUID, Instance Replicated)] -> m UUID
newObject fields = do
    payloads <- for fields $ \(_, Instance value) -> newRon value
    event <- getEventUuid
    modify' $
        (<>) $ Map.singleton event $
        StateChunk
            { stateType = lwwType
            , stateBody =
                [Op event name p | ((name, _), p) <- zip fields payloads]
            }
    pure event

-- | Decode field value
viewField
    :: (Replicated a, MonadE m, MonadState StateFrame m)
    => UUID        -- ^ Field name
    -> StateChunk  -- ^ LWW object chunk
    -> m a
viewField field StateChunk{..} =
    errorContext ("LWW.viewField " <> show field) $ do
        let ops = filter (\Op{refId} -> refId == field) stateBody
        Op{payload} <- case ops of
            []   -> throwError "no field in lww chunk"
            [op] -> pure op
            _    -> throwError "unreduced state"
        fromRon payload

-- | Decode field value
readField
    :: (MonadE m, MonadObjectState struct m, Replicated field)
    => UUID  -- ^ Field name
    -> m field
readField field = do
    stateChunk <- getObjectStateChunk
    viewField field stateChunk

-- | Assign a value to a field
assignField
    :: (Replicated field, ReplicaClock m, MonadE m, MonadObjectState struct m)
    => UUID   -- ^ Field name
    -> field  -- ^ Value
    -> m ()
assignField field value = do
    StateChunk{stateBody} <- getObjectStateChunk
    advanceToObject
    let chunk = filter (\Op{refId} -> refId /= field) stateBody
    event <- getEventUuid
    p <- newRon value
    let newOp = Op event field p
    let chunk' = sortOn refId $ newOp : chunk
    let state' = StateChunk{stateBody = chunk', stateType = lwwType}
    Object uuid <- ask
    modify' $ Map.insert uuid state'

-- | Pseudo-lens to an object inside a specified field
zoomField
    :: MonadE m
    => UUID                     -- ^ Field name
    -> ObjectStateT field  m a  -- ^ Inner object modifier
    -> ObjectStateT struct m a
zoomField field innerModifier =
    errorContext ("LWW.zoomField" <> show field) $ do
        StateChunk{stateBody} <- getObjectStateChunk
        let ops = filter (\Op{refId} -> refId == field) stateBody
        Op{payload} <- case ops of
            []   -> throwError "empty chunk"
            [op] -> pure op
            _    -> throwError "unreduced state"
        fieldObjectId <- errorContext "inner object" $ case payload of
            [AUuid oid] -> pure oid
            _           -> throwError "Expected object UUID"
        lift $ runReaderT innerModifier $ Object fieldObjectId
