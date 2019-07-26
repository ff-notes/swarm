{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Replicated Growable Array (RGA)
module RON.Data.RGA
    ( RGA (..)
    , RgaRaw
    , RgaString
    , edit
    , editText
    , getAliveIndices
    , getList
    , getText
    , insert
    , insertAfter
    , insertAtBegin
    , insertText
    , insertTextAfter
    , insertTextAtBegin
    , newFromList
    , newFromText
    , remove
    , rgaType
    ) where

import           RON.Prelude

import           Data.Algorithm.Diff (Diff (Both, First, Second),
                                      getGroupedDiffBy)
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           RON.Data.Internal (MonadObjectState,
                                    ReducedChunk (ReducedChunk), Reducible,
                                    Replicated, ReplicatedAsObject,
                                    ReplicatedAsPayload, Unapplied,
                                    applyPatches, encoding, fromRon, getObject,
                                    getObjectStateChunk,
                                    modifyObjectStateChunk_, newObject, newRon,
                                    objectEncoding, objectOpType, rcBody, rcRef,
                                    reduceUnappliedPatches, reducibleOpType,
                                    stateFromChunk, stateToChunk, toPayload)
import           RON.Error (MonadE, errorContext, throwErrorText)
import           RON.Event (ReplicaClock, getEventUuid, getEventUuids)
import           RON.Types (Object (Object), Op (..), StateChunk (..),
                            StateFrame, UUID)
import           RON.Util.Word (pattern B11, ls60)
import           RON.UUID (pattern Zero, uuidVersion)
import qualified RON.UUID as UUID

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- | opId = vertex id
--   refId:
--      0 = value is alive,
--      _ = tombstone event, value is backup for undo
--   payload: the value
type Vertex = Op

data VertexListItem = VertexListItem
    { itemValue :: Vertex
    , itemNext  :: Maybe UUID
    }
    deriving (Eq, Show)

data VertexList = VertexList
    { listHead  :: UUID
    , listItems :: HashMap UUID VertexListItem
    }
    deriving (Eq, Show)

instance Semigroup VertexList where
    (<>) = merge

vertexListToOps :: VertexList -> [Vertex]
vertexListToOps v@VertexList{..} = go listHead listItems
  where
    go root items = let
        VertexListItem{..} =
            HashMap.lookupDefault
                (error $ unlines
                    $  ["Cannot find vertex id", show root, "in array"]
                    ++ map show (HashMap.toList items)
                    ++ ["Original array is", show v])
                root
                items
        rest = case itemNext of
            Just next -> go next (HashMap.delete root items)
            Nothing -> []
        in itemValue : rest

vertexListFromOps :: [Vertex] -> Maybe VertexList
vertexListFromOps = foldr go mempty where
    go v@Op{opId} vlist = Just $ VertexList{listHead = opId, listItems = vlist'}
      where
        item itemNext = VertexListItem{itemValue = v, itemNext}
        vlist' = case vlist of
            Nothing -> HashMap.singleton opId (item Nothing)
            Just VertexList{listHead, listItems} ->
                HashMap.insert opId (item $ Just listHead) listItems

-- | Untyped RGA
newtype RgaRaw = RgaRaw (Maybe VertexList)
    deriving (Eq, Monoid, Semigroup, Show)

data PatchSet = PatchSet
    { psPatches  :: Map UUID VertexList
        -- ^ the key is the parent event, the value is a non-empty VertexList
    , psRemovals :: Map UUID UUID
        -- ^ the key is the target event, the value is the tombstone event
    }
    deriving (Eq, Show)

instance Semigroup PatchSet where
    rga1 <> rga2 = reapplyPatchSet $ preMerge rga1 rga2

preMerge :: PatchSet -> PatchSet -> PatchSet
preMerge (PatchSet p1 r1) (PatchSet p2 r2) = PatchSet
    {psPatches = Map.unionWith (<>) p1 p2, psRemovals = Map.unionWith max r1 r2}

instance Monoid PatchSet where
    mempty = PatchSet{psPatches = mempty, psRemovals = mempty}

patchSetFromRawOp :: Op -> PatchSet
patchSetFromRawOp op@Op{opId, refId, payload} = case payload of
    [] ->  -- remove op
        mempty{psRemovals = Map.singleton refId opId}
    _:_ ->  -- append op
        mempty
            { psPatches =
                Map.singleton
                    refId
                    VertexList
                        { listHead = opId
                        , listItems =
                            HashMap.singleton
                                opId
                                VertexListItem
                                    { itemValue = op{refId = Zero}
                                    , itemNext  = Nothing
                                    }
                        }
            }

patchSetFromChunk :: ReducedChunk -> PatchSet
patchSetFromChunk ReducedChunk{rcRef, rcBody} =
    case uuidVersion $ UUID.split rcRef of
        B11 ->
            -- derived event -- rm-patch compatibility
            foldMap patchSetFromRawOp rcBody
        _ ->  -- patch
            case vertexListFromOps rcBody of
                Just patch -> mempty{psPatches = Map.singleton rcRef patch}
                Nothing -> mempty

instance Reducible RgaRaw where
    reducibleOpType = rgaType

    stateFromChunk = RgaRaw . vertexListFromOps

    stateToChunk (RgaRaw rga) = StateChunk{stateType = rgaType, stateBody} where
        stateBody = maybe [] vertexListToOps rga

    applyPatches rga (patches, ops) =
        bimap id patchSetToChunks . reapplyPatchSetToState rga $
        foldMap patchSetFromChunk patches <> foldMap patchSetFromRawOp ops

    reduceUnappliedPatches (patches, ops) =
        patchSetToChunks . reapplyPatchSet $
        foldMap patchSetFromChunk patches <> foldMap patchSetFromRawOp ops

patchSetToChunks :: PatchSet -> Unapplied
patchSetToChunks PatchSet{psPatches, psRemovals} =
    (   [ ReducedChunk{..}
        | (rcRef, vertices) <- Map.assocs psPatches
        , let rcBody = vertexListToOps vertices
        ]
    ,   [ Op{opId = tombstone, refId, payload = []}
        | (refId, tombstone) <- Map.assocs psRemovals
        ]
    )

reapplyPatchSet :: PatchSet -> PatchSet
reapplyPatchSet ps =
    continue ps [reapplyPatchesToOtherPatches, reapplyRemovalsToPatches]

reapplyPatchSetToState :: RgaRaw -> PatchSet -> (RgaRaw, PatchSet)
reapplyPatchSetToState rga ps =
    continue (rga, ps) [reapplyPatchesToState, reapplyRemovalsToState]

continue :: x -> [x -> Maybe x] -> x
continue x fs = case asum $ map ($ x) fs of
    Nothing -> x
    Just x' -> continue x' fs

reapplyPatchesToState :: (RgaRaw, PatchSet) -> Maybe (RgaRaw, PatchSet)
reapplyPatchesToState (RgaRaw rstate, ps@PatchSet{..}) = case rstate of
    Just VertexList{listHead = targetHead, listItems = targetItems} -> asum
        [ do
            targetItems' <- applyPatch parent patch targetItems
            pure
                ( RgaRaw . Just $ VertexList targetHead targetItems'
                , ps{psPatches = Map.delete parent psPatches}
                )
        | (parent, patch) <- Map.assocs psPatches
        ]
    Nothing -> do
        -- rstate is empty => only virtual 0 node exists
        -- => we can apply only 0 patch
        patch <- psPatches !? Zero
        pure (RgaRaw $ Just patch, ps{psPatches = Map.delete Zero psPatches})

reapplyPatchesToOtherPatches :: PatchSet -> Maybe PatchSet
reapplyPatchesToOtherPatches ps@PatchSet{..} = asum
    [ do
        targetItems' <- applyPatch parent patch targetItems
        pure ps
            { psPatches =
                Map.insert targetParent (VertexList targetHead targetItems') $
                Map.delete parent psPatches
            }
    | (parent, patch) <- Map.assocs psPatches
    , (targetParent, targetPatch) <- Map.assocs psPatches
    , parent /= targetParent
    , let VertexList targetHead targetItems = targetPatch
    ]

applyPatch
    :: UUID
    -> VertexList
    -> HashMap UUID VertexListItem
    -> Maybe (HashMap UUID VertexListItem)
applyPatch parent patch targetItems = case parent of
    Zero ->
        error "chunk with zero ref mustn't be considered a state, not a patch"
    _ -> do
        item@VertexListItem{itemNext} <- HashMap.lookup parent targetItems
        let VertexList next' newItems = case itemNext of
                Nothing   -> patch
                Just next -> VertexList next targetItems <> patch
        let item' = item{itemNext = Just next'}
        pure $ HashMap.insert parent item' targetItems <> newItems

reapplyRemovalsToState :: (RgaRaw, PatchSet) -> Maybe (RgaRaw, PatchSet)
reapplyRemovalsToState (RgaRaw rstate, ps@PatchSet{..}) = do
    VertexList{listHead = targetHead, listItems = targetItems} <- rstate
    asum
        [ do
            targetItems' <- applyRemoval parent tombstone targetItems
            pure
                ( RgaRaw . Just $ VertexList targetHead targetItems'
                , ps{psRemovals = Map.delete parent psRemovals}
                )
        | (parent, tombstone) <- Map.assocs psRemovals
        ]

reapplyRemovalsToPatches :: PatchSet -> Maybe PatchSet
reapplyRemovalsToPatches PatchSet{..} = asum
    [ do
        targetItems' <- applyRemoval parent tombstone targetItems
        pure PatchSet
            { psRemovals = Map.delete parent psRemovals
            , psPatches =
                Map.insert
                    targetParent (VertexList targetHead targetItems') psPatches
            }
    | (parent, tombstone) <- Map.assocs psRemovals
    , (targetParent, targetPatch) <- Map.assocs psPatches
    , let VertexList targetHead targetItems = targetPatch
    ]

applyRemoval
    :: UUID
    -> UUID
    -> HashMap UUID VertexListItem
    -> Maybe (HashMap UUID VertexListItem)
applyRemoval parent tombstone targetItems = do
    item@VertexListItem{itemValue = v@Op{refId}} <-
        HashMap.lookup parent targetItems
    let item' = item{itemValue = v{refId = max refId tombstone}}
    pure $ HashMap.insert parent item' targetItems

merge :: VertexList -> VertexList -> VertexList
merge v1 v2
    = fromMaybe (error "merge of non-empty lists cannot be empty")
    $ vertexListFromOps
    $ (merge' `on` vertexListToOps) v1 v2

merge' :: [Vertex] -> [Vertex] -> [Vertex]
merge' [] vs2 = vs2
merge' vs1 [] = vs1
merge' w1@(v1 : vs1) w2@(v2 : vs2) =
    case compare e1 e2 of
        LT -> v2 : merge' w1 vs2
        GT -> v1 : merge' vs1 w2
        EQ -> mergeVertices : merge' vs1 vs2
  where
    Op{opId = e1, refId = tombstone1, payload = p1} = v1
    Op{opId = e2, refId = tombstone2, payload = p2} = v2

    -- priority of deletion
    mergeVertices = Op
        { opId    = e1
        , refId   = max tombstone1 tombstone2
        , payload = maxOn length p1 p2
        }

-- | Name-UUID to use as RGA type marker.
rgaType :: UUID
rgaType = $(UUID.liftName "rga")

-- | Typed RGA
newtype RGA a = RGA [a]
    deriving (Eq, Show)

instance Replicated a => Replicated (RGA a) where encoding = objectEncoding

instance Replicated a => ReplicatedAsObject (RGA a) where
    objectOpType = rgaType

    newObject (RGA items) = do
        vertexIds <- getEventUuids $ ls60 $ genericLength items
        ops <- for (zip items vertexIds) $ \(item, vertexId) -> do
            payload <- newRon item
            pure $ Op vertexId Zero payload
        oid <- getEventUuid
        modify' $
            (<>) $ Map.singleton oid $
            StateChunk{stateType = rgaType, stateBody = ops}
        pure $ Object oid

    getObject = do
        StateChunk{stateBody} <- getObjectStateChunk
        mItems <- for stateBody $ \Op{refId, payload} -> case refId of
            Zero -> Just <$> fromRon payload
            _    -> pure Nothing
        pure . RGA $ catMaybes mItems

-- | Replace content of the RGA throug introducing changes detected by
-- 'getGroupedDiffBy'.
edit
    ::  ( ReplicatedAsPayload a
        , ReplicaClock m, MonadE m, MonadObjectState (RGA a) m
        )
    => [a] -> m ()
edit newItems =
    modifyObjectStateChunk_ $ \chunk@StateChunk{stateBody} -> do
        let newItems' = [Op Zero Zero $ toPayload item | item <- newItems]
            -- TODO(2019-04-17, #59, cblp) replace 'toPayload' with 'newRon' and
            -- relax constraint on 'a' from 'ReplicatedAsPayload' to
            -- 'Replicated'
        let diff = getGroupedDiffBy eqAliveOnPayload stateBody newItems'
        (stateBody', Last lastEvent) <-
            runWriterT . fmap fold . for diff $ \case
                First removed -> for removed $ \case
                    op@Op{refId = Zero} -> do  -- not deleted yet
                        -- TODO(2018-11-03, #15, cblp) get sequential ids
                        tombstone <- getEventUuid
                        tell . Last $ Just tombstone
                        pure op{refId = tombstone}
                    op ->  -- deleted already
                        pure op
                Both v _     -> pure v
                Second added -> for added $ \op -> do
                    -- TODO(2018-11-03, #15, cblp) get sequential ids
                    opId <- getEventUuid
                    tell . Last $ Just opId
                    pure op{opId}
        case lastEvent of
            Nothing -> pure chunk
            Just _  ->
                pure StateChunk{stateType = rgaType, stateBody = stateBody'}
  where
    eqAliveOnPayload
            Op{refId = Zero, payload = p1}
            Op{refId = Zero, payload = p2}
        = p1 == p2
    eqAliveOnPayload _ _ = False

-- | Speciaization of 'edit' for 'Text'
editText
    :: (ReplicaClock m, MonadE m, MonadObjectState RgaString m)
    => Text -> m ()
editText = edit . Text.unpack

-- | Speciaization of 'RGA' to 'Char'.
-- This is the recommended way to store a string.
type RgaString = RGA Char

-- | Create an RGA from a list
newFromList
    :: (Replicated a, MonadState StateFrame m, ReplicaClock m)
    => [a] -> m (Object (RGA a))
newFromList = newObject . RGA

-- | Create an 'RgaString' from a text
newFromText
    :: (MonadState StateFrame m, ReplicaClock m)
    => Text -> m (Object RgaString)
newFromText = newFromList . Text.unpack

getAliveIndices :: (MonadE m, MonadObjectState (RGA a) m) => m [UUID]
getAliveIndices = do
    StateChunk{stateBody} <- getObjectStateChunk
    let mItems =
            [ case refId of
                Zero -> Just opId
                _    -> Nothing
            | Op{opId, refId} <- stateBody
            ]
    pure $ catMaybes mItems

-- | Read elements from RGA
getList :: (Replicated a, MonadE m, MonadObjectState (RGA a) m) => m [a]
getList = coerce <$> getObject

-- | Read characters from 'RgaString'
getText :: (MonadE m, MonadObjectState RgaString m) => m Text
getText = Text.pack <$> getList

-- | Insert a sequence of elements after the specified position.
-- Position is identified by 'UUID'. 'Nothing' means the beginning.
insert
    :: (Replicated a, MonadE m, MonadObjectState (RGA a) m, ReplicaClock m)
    => [a]
    -> Maybe UUID  -- ^ position
    -> m ()
insert []    _         = pure ()
insert items mPosition =
    modifyObjectStateChunk_ $ \chunk@StateChunk{stateBody} -> do
        vertexIds <- getEventUuids $ ls60 $ genericLength items
        ops <-
            for (zip items vertexIds) $ \(item, vertexId) -> do
                payload <- newRon item
                pure $ Op vertexId Zero payload
        stateBody' <- case mPosition of
            Nothing -> pure $ ops <> stateBody
            Just position -> findAndInsertAfter position ops stateBody
        pure chunk{stateBody = stateBody'}
  where
    findAndInsertAfter pos newOps = go where
        go = \case
            []                -> throwErrorText "Position not found"
            op@Op{opId} : ops
                | opId == pos -> pure $ op : newOps ++ ops
                | otherwise   -> (op :) <$> go ops

insertAtBegin
    :: (Replicated a, MonadE m, MonadObjectState (RGA a) m, ReplicaClock m)
    => [a] -> m ()
insertAtBegin items = insert items Nothing

insertAfter
    :: (Replicated a, MonadE m, MonadObjectState (RGA a) m, ReplicaClock m)
    => [a]
    -> UUID  -- ^ position
    -> m ()
insertAfter items = insert items . Just

-- | Insert a text after the specified position.
-- Position is identified by 'UUID'. 'Nothing' means the beginning.
insertText
    :: (ReplicaClock m, MonadE m, MonadObjectState RgaString m)
    => Text
    -> Maybe UUID  -- ^ position
    -> m ()
insertText = insert . Text.unpack

insertTextAtBegin
    :: (ReplicaClock m, MonadE m, MonadObjectState RgaString m) => Text -> m ()
insertTextAtBegin text = insertText text Nothing

insertTextAfter
    :: (ReplicaClock m, MonadE m, MonadObjectState RgaString m)
    => Text
    -> UUID  -- ^ position
    -> m ()
insertTextAfter text = insertText text . Just

-- | Record a removal of a specific item
remove
    :: (MonadE m, MonadObjectState (RGA a) m, ReplicaClock m)
    => UUID  -- ^ position
    -> m ()
remove position =
    errorContext "RGA.remove" $
    errorContext ("position = " <> show position) $
    modifyObjectStateChunk_ $ \chunk@StateChunk{stateBody} -> do
        event <- getEventUuid
        stateBody' <- findAndTombstone event stateBody
        pure chunk{stateBody = stateBody'}
  where
    findAndTombstone event = go where
        go = \case
            []                     -> throwErrorText "Position not found"
            op@Op{opId} : ops
                | opId == position -> pure $ op{refId = event} : ops
                | otherwise        -> (op :) <$> go ops
