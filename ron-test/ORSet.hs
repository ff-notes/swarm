{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ORSet (prop_orSet) where

import           RON.Prelude

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String.Interpolate.IsString (i)
import           Hedgehog (Property, evalEither, evalExceptT, property, (===))

import           RON.Data (evalObjectState, execObjectState, getObject,
                           newObjectState)
import           RON.Data.ORSet (ORSet (ORSet), addValue, findAnyAlive',
                                 removeValue, zoom)
import           RON.Event (ReplicaId, applicationSpecific)
import           RON.Event.Simulation (runNetworkSim, runNetworkSimT,
                                       runReplicaSim, runReplicaSimT)
import           RON.Schema.TH (mkReplicated)
import           RON.Text (parseObject, serializeObject)

import           Orphans ()

[mkReplicated|
    (alias Set27
        (ORSet                      ; top-level set of objects
            (ORSet                  ; nested set of object
                (ORSet String))))   ; nested set of atoms
|]

example0 :: Set27
example0 = ORSet [ORSet [ORSet ["octaves"]]]

state1expect :: BSL.ByteString
state1expect = [i|
    *set    #B/00009NFGUW+r3pl1c4   @`(6g0dUW   !
                                    @           'octaves'
            #(AvZ0UW                @(4bX_UW    !
                                    @           >(9NFGUW
            #(EYD0UW                @(1KqirW    !
                                    @           >(AvZ0UW
    .
    |]

example4expect :: Set27
example4expect = ORSet [ORSet [ORSet ["candies"]], ORSet []]

prop_orSet :: Property
prop_orSet = property $ do
    -- create an object
    let state1 = runNetworkSim $ runReplicaSim replica $ newObjectState example0
    let (oid, state1ser) = serializeObject state1
    prep state1expect === prep state1ser

    -- parse newly created object
    state2 <- evalEither $ parseObject oid state1ser
    state1 === state2

    -- decode newly created object
    example3 <- evalEither $ evalObjectState state2 getObject
    example0 === example3

    -- apply operations to the object (frame)
    state4 <-
        evalExceptT $
        runNetworkSimT $ runReplicaSimT replica $
        execObjectState state2 $ do
            -- top-level
            addValue $ ORSet []
            set1 <- findAnyAlive'
            zoom set1 $ do
                set2 <- findAnyAlive'
                zoom set2 $ do
                    addValue "candies"
                    removeValue "octaves"

    -- decode object after modification
    example4 <- evalEither $ evalObjectState state4 getObject
    example4expect === example4

--     -- serialize object after modification
--     prep ex4expect === prep (snd $ serializeObject state4)

  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines

-- | "r3pl1c4"
replica :: ReplicaId
replica = applicationSpecific 0xd83d30067100000
