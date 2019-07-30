{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module StructSet (prop_structSet) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String.Interpolate.IsString (i)
import           Hedgehog (Property, evalEither, evalExceptT, property, (===))

import           RON.Data (evalObjectState, getObject, newObjectFrame,
                           runObjectState)
import           RON.Data.ORSet (ORSet (ORSet))
import qualified RON.Data.ORSet as ORSet
import           RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import           RON.Event (ReplicaId, applicationSpecific)
import           RON.Event.Simulation (runNetworkSimT, runReplicaSimT)
import           RON.Text (parseObject, serializeObject)
import           RON.Util (ByteStringL)

import           StructSet.Types

example0 :: StructSet13
example0 = StructSet13
    { int1 = [275]
    , str2 = [RGA "275"]
    , str3 = ["190"]
    , set4 = [ORSet []]
    , opt5 = [Nothing]
    , nst6 = []
    }

-- | "r3pl1c4"
replica :: ReplicaId
replica = applicationSpecific 0xd83d30067100000

state1expect :: ByteStringL
state1expect = [i|
    *rga    #B/0000GrVLcW+r3pl1c4               !
                                    @`(EWD0g6   '2'
                                    @)7         '7'
                                    @)8         '5'

    *set    #(9NFGUW                @0          !

            #(IdJV2W                            !
                                    @`(1KqirW   >int1   275
                                    @(4bX_UW    >opt5   >none
                                    @(6g0dUW    >set4   >B/00009NFGUW+r3pl1c4
                                    @(AvZ0UW    >str2   >B/0000GrVLcW+r3pl1c4
                                    @(IGnZdW    >str3   '190'
    .
    |]

-- | TODO (2019-07-30, cblp) BUG! #(IdJV2W @` :0 >int1 166 -- impossible
state4expect :: ByteStringL
state4expect = [i|
    *rga    #B/0000GrVLcW+r3pl1c4                           !
                                    @`(EWD0g6   :`(PRyXUW   '2'
                                    @)7         :(RgJfUW    '7'
                                    @(SxA_UW    :0          '1'
                                    @(TEddUW                '4'
                                    @(EWD0g8                '5'

            #(eQ~LcW                @0                      !
                                    @`[JT0g6                '1'
                                    @)7                     '3'
                                    @)8                     '6'

    *set    #(9NFGUW                @0                      !
                                    @`(jil2MW               >(fxJV2W

            #(IdJV2W                @0                      !
                                    @`(1KqirW   :`(MDl2MW   >int1
                                    @(4bX_UW    :0          >opt5 >none
                                    @(6g0dUW                >set4 >B/00009NFGUW+r3pl1c4
                                    @(AvZ0UW                >str2 >B/0000GrVLcW+r3pl1c4
                                    @(IGnZdW    :`(_s30UW   >str3
                                    @`          :0          >int1 166
                                    @(X6VGUW                >str3 '206'
                                    @(mRyXUW                >nst6 >B/0000pxA_UW+r3pl1c4

            #(fxJV2W                @0                      !
                                    @`(dMD0UW               >int1 135
                                    @[nL0UW                 >str2 >B/0000eQ~LcW+r3pl1c4
                                    @(frnZdW                >str3 '137'

            #(pxA_UW                @0                      !
                                    @`(ogJfUW               >int1 138
    .
    |]

example4expect :: StructSet13
example4expect = StructSet13
    { int1 = [166]
    , str2 = [RGA "145"]
    , str3 = ["206"]
    , set4 = [ORSet [mempty{int1 = [135], str2 = [RGA "136"], str3 = ["137"]}]]
    , opt5 = [Nothing]
    , nst6 = [mempty{int1 = [138]}]
    }

prop_structSet :: Property
prop_structSet = property $ do
    -- create an object
    state1 <- runNetworkSimT $ runReplicaSimT replica $ newObjectFrame example0
    let (oid, state1ser) = serializeObject state1
    prep state1expect === prep state1ser

    -- parse newly created object
    state2 <- evalEither $ parseObject oid state1ser
    state1 === state2

    -- decode newly created object
    example3 <- evalEither $ evalObjectState state2 getObject
    example0 === example3

    -- apply operations to the object (frame)
    ((str3Value, opt5Value, nst6Value), state4) <-
        evalExceptT $
        runNetworkSimT $ runReplicaSimT replica $
        runObjectState state2 $ do
            -- plain field
            int1_assign 166
            str2_zoom $ RGA.edit "145"
            str3Value <- str3_read
            str3_assign "206"
            set4_zoom $
                ORSet.addValue
                    mempty{int1 = [135], str2 = [RGA "136"], str3 = ["137"]}
            opt5Value <- opt5_read
            nst6Value <- nst6_read
            nst6_assign mempty{int1 = [138]}
            pure (str3Value, opt5Value, nst6Value)
    str3Value === ["190"]
    opt5Value === [Nothing]
    nst6Value === []

    -- decode object after modification
    example4 <- evalEither $ evalObjectState state4 getObject
    example4expect === example4

    -- serialize object after modification
    prep state4expect === prep (snd $ serializeObject state4)

  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines
