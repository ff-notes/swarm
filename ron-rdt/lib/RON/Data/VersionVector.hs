{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Version Vector
module RON.Data.VersionVector
    ( VersionVector
    ) where

import           RON.Prelude

import           Data.Hashable (hashWithSalt)
import qualified Data.Map.Strict as Map

import           RON.Data.Internal
import           RON.Event (getEventUuid)
import           RON.Semilattice (BoundedSemilattice, Semilattice)
import           RON.Types (Object (Object), Op (..), StateChunk (..),
                            UUID (UUID))
import qualified RON.UUID as UUID

type Origin = Word64

opTime :: Op -> Word64
opTime Op{opId = UUID time _} = time

opOrigin :: Op -> Word64
opOrigin Op{opId = UUID _ origin} = origin

latter :: Op -> Op -> Op
latter = maxOn opTime

-- | Version Vector type. May be used both in typed and untyped contexts.
newtype VersionVector = VersionVector (Map Origin Op)
    deriving (Eq, Show)

instance Hashable VersionVector where
    hashWithSalt s (VersionVector vv) = hashWithSalt s $ Map.assocs vv

instance Semigroup VersionVector where
    (<>) = coerce $ Map.unionWith latter

instance Monoid VersionVector where
    mempty = VersionVector mempty

instance Semilattice VersionVector

instance BoundedSemilattice VersionVector

instance Reducible VersionVector where
    reducibleOpType = vvType

    stateFromChunk ops =
        VersionVector $ Map.fromListWith latter [(opOrigin op, op) | op <- ops]

    stateToChunk (VersionVector vv) = mkStateChunk vvType $ Map.elems vv

-- | Name-UUID to use as Version Vector type marker.
vvType :: UUID
vvType = $(UUID.liftName "vv")

instance Replicated VersionVector where
    encoding = objectEncoding

instance ReplicatedBoundedSemilattice VersionVector where
    rconcat = objectRconcat

instance ReplicatedAsObject VersionVector where
    type Rep VersionVector = VersionVector

    newObject (VersionVector vv) = do
        oid <- getEventUuid
        let ops = Map.elems vv
        let stateVersion = maximumDef oid $ map opId ops
        modify' $
            (<>) $ Map.singleton oid $
            StateChunk{stateType = vvType, stateVersion, stateBody = ops}
        pure $ Object oid

    getObject = do
        StateChunk{stateBody} <- getObjectStateChunk
        pure $ stateFromChunk stateBody
