{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module RON.Store.Test (emptyDB, runStoreSim) where

import           RON.Prelude

import           Control.Lens (at, non, zoom, (.=), (<>=))
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import           RON.Error (Error)
import           RON.Event (ReplicaClock, ReplicaId, applicationSpecific)
import           RON.Event.Simulation (ReplicaSimT, runNetworkSimT,
                                       runReplicaSimT)
import           RON.Store (MonadStore (..))
import           RON.Types (Op, StateChunk (..), UUID)
import           RON.Types.Experimental (CollectionName, ObjectRef (..))

type Collection = Map ObjectId Object

data Object = Object
    { value :: [Op]
    , logs  :: ObjectLogs
    }
    deriving (Eq, Generic, Show)

emptyObject :: Object
emptyObject = Object{value = [], logs = HashMap.empty}

type ObjectId = UUID

type ObjectLogs = HashMap ReplicaId (Seq Op)

newtype TestDB = TestDB
    { collections :: Map CollectionName Collection
    }
    deriving (Generic, Show)

emptyDB :: TestDB
emptyDB = TestDB{collections = Map.empty}

newtype StoreSim a = StoreSim (StateT TestDB (ReplicaSimT (Either Error)) a)
    deriving (Applicative, Functor, Monad, MonadError Error, ReplicaClock)

runStoreSim :: TestDB -> StoreSim a -> Either Error (a, TestDB)
runStoreSim db (StoreSim action) =
    runNetworkSimT $ runReplicaSimT thisReplicaId $ runStateT action db

thisReplicaId :: ReplicaId
thisReplicaId = applicationSpecific 2020

instance MonadStore StoreSim where
    getCollections = StoreSim $ gets $ Map.keys . collections

    getObjects collection =
        StoreSim $ do
            TestDB{collections} <- get
            pure $ map (ObjectRef collection) $ Map.keys $
                collections !. collection

    loadObjectChunk (ObjectRef collection objectId) =
        StoreSim $ do
            TestDB{collections} <- get
            pure $ fmap (StateChunk . \Object{value} -> value) $
                Map.lookup objectId $ collections !. collection

    savePatchAndObjectChunk
        (ObjectRef collection objectId)
        (patch, StateChunk value)
        =
            StoreSim $
                atCollection . atObject `zoom` do
                    #value .= value
                    #logs . at thisReplicaId . non Seq.empty <>= patch
        where
            atCollection = #collections . at collection . non Map.empty
            atObject = at objectId . non emptyObject

(!.) :: Ord a => Map a (Map b c) -> a -> Map b c
m !. a = fromMaybe Map.empty $ m !? a
