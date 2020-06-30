{-# OPTIONS_GHC -Wwarn=redundant-constraints #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Data.ORSet.Experimental (
  ORSet,
  ORMap,
  add,
  add_,
  lookupLww,
  toList,
) where

import           RON.Prelude hiding (toList)

import qualified Data.Map.Strict as Map

import           RON.Data.Experimental (AsAtom, AsAtoms, Rep, Replicated,
                                        ReplicatedObject, fromAtoms,
                                        replicatedTypeId, stateFromFrame,
                                        toAtom, toAtoms, view)
import           RON.Data.ORSet (setType)
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock, advanceToUuid, getEventUuid)
import           RON.Store (MonadStore, appendPatch)
import           RON.Types (ObjectRef (..), Op (..), Payload, UUID)

-- | Observed-Remove Set.
-- Implementation: a map from the itemId to the original op.
-- Each time a value is added, a new item=op is created.
-- Deletion of a value replaces all its known items with tombstone ops.
newtype ORSet a = ORSet (Map UUID (UUID, Payload))
  deriving (Eq, Show)

instance Replicated (ORSet a) where
  replicatedTypeId = setType
  stateFromFrame objectId = ORSet . \case
    [] -> Map.empty
    ops ->
      Map.fromListWith
        (maxOn fst)
        [ (itemId, (opId, payload))
        | Op{opId, refId, payload} <- ops
        , opId /= objectId
        , let
          itemId
            | refId == objectId = opId
            | otherwise         = refId
        ]

instance ReplicatedObject (ORSet a) where
  type Rep (ORSet a) = ORSet a
  view _id = pure

-- | Add value to the set. Return the reference to the set item.
add ::
  (Rep container ~ ORSet item, AsAtoms item, MonadStore m, ReplicaClock m) =>
  ObjectRef container -> item -> m UUID
add (ObjectRef object) value = do
  advanceToUuid object
  opId <- getEventUuid
  appendPatch object [Op{opId, refId = object, payload = toAtoms value}]
  pure opId

{- |
  Add value to the set or map.

  @add_ :: ObjectRef (ORSet a)   -> a      -> m ()@
  @add_ :: ObjectRef (ORMap k v) -> (k, v) -> m ()@
  -}
add_ ::
  (Rep container ~ ORSet item, AsAtoms item, MonadStore m, ReplicaClock m) =>
  ObjectRef container -> item -> m ()
add_ objectRef payload = void $ add objectRef payload

toList :: (AsAtoms a, MonadE m) => ORSet a -> [m a]
toList (ORSet rep) =
  [fromAtoms payload | (_item, payload@(_:_)) <- Map.elems rep]

type ORMap k v = ORSet (k, v)

lookupLww :: (AsAtom k, AsAtoms v, MonadE m) => k -> ORMap k v -> m (Maybe v)
lookupLww key (ORSet s) = do
  traverse fromAtoms $
    snd <$>
    maximumMayOn
      fst
      [(item, value) | (item, k : value) <- Map.elems s, k == toAtom key]
