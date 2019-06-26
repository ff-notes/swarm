{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module StructSet (prop_structSet) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String.Interpolate.IsString (i)
import           Hedgehog (Property, evalEither, evalExceptT, property, (===))

import           RON.Data (evalObjectState, getObject, newObjectState,
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
    *rga    #B/0000GrVLcW+r3pl1c4   @`(EWD0g8   !
                                    @)6         '2'
                                    @)7         '7'
                                    @)8         '5'

    *set    #(9NFGUW                @`          !

            #(IdJV2W                @`          !
                                    @(1KqirW    >int1   275
                                    @(4bX_UW    >opt5   >none
                                    @(6g0dUW    >set4   >B/00009NFGUW+r3pl1c4
                                    @(AvZ0UW    >str2   >B/0000GrVLcW+r3pl1c4
                                    @(IGnZdW    >str3   '190'
    .
    |]
-- TODO(2019-07-23, cblp) why not compressed to ">str2 >0000GrVLcW"?

state4expect :: ByteStringL
state4expect = [i|
    *rga    #B/0000GrVLcW+r3pl1c4   @`(X6VGUW               !
                                    @(EWD0g6    :`(RgJfUW   '2'
                                    @)7         :(SxA_UW    '7'
                                    @(TEddUW    :0          '1'
                                    @(X6VGUW                '4'
                                    @(EWD0g8                '5'

            #(kG~LcW                @(hOy0g8                !
                                    @)6                     '1'
                                    @)7                     '3'
                                    @)8                     '6'

    *set    #(9NFGUW                @(oil2MW                !
                                    @                       >(oCJV2W

            #(IdJV2W                @(qRyXUW                !
                                    @(1KqirW    :`(PRyXUW   >int1
                                    @(4bX_UW    :0          >opt5 >none
                                    @(6g0dUW                >set4 >B/00009NFGUW+r3pl1c4
                                    @(AvZ0UW                >str2 >B/0000GrVLcW+r3pl1c4
                                    @(IGnZdW    :`(dMD0UW   >str3
                                    @(MDl2MW    :0          >int1 166
                                    @(_s30UW                >str3 '206'
                                    @(qRyXUW                >nst6 >B/0000txA_UW+r3pl1c4

            #(oCJV2W                @`                      !
                                    @(dnL0UW                >int1 135
                                    @(eLa0UW                >str2 >B/0000kG~LcW+r3pl1c4
                                    @(n2nZdW                >str3 '137'

            #(txA_UW                @`                      !
                                    @(sgJfUW                >int1 138
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
    state1 <- runNetworkSimT $ runReplicaSimT replica $ newObjectState example0
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
