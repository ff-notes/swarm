{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LwwStruct (prop_lwwStruct) where

import           RON.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String.Interpolate.IsString (i)
import           Hedgehog (Property, evalEither, evalExceptT, property, (===))

import           RON.Data (evalObjectState, execObjectState, getObject,
                           newObjectState)
import           RON.Data.ORSet (ORSet (ORSet))
import qualified RON.Data.ORSet as ORSet
import           RON.Data.RGA (RGA (RGA))
import qualified RON.Data.RGA as RGA
import           RON.Event (ReplicaId, applicationSpecific)
import           RON.Event.Simulation (runNetworkSim, runNetworkSimT,
                                       runReplicaSim, runReplicaSimT)
import           RON.Text (parseObject, serializeObject)
import           RON.Util (ByteStringL)

import           LwwStruct.Types (Struct51 (..), int1_assign, opt5_read,
                                  opt6_assign, opt6_read, set4_zoom, str2_zoom,
                                  str3_assign, str3_read)
import           Orphans ()

example0 :: Struct51
example0 = Struct51
    { int1 = 275
    , str2 = RGA "275"
    , str3 = "190"
    , set4 = ORSet []
    , opt5 = Nothing
    , opt6 = Just 74
    }

-- | "r3pl1c4"
replica :: ReplicaId
replica = applicationSpecific 0xd83d30067100000

ex1expect :: ByteStringL
ex1expect = [i|
    *lww    #B/00009ISodW+r3pl1c4   @`                  !
                                                :int1   275
                                                :opt5   >none
                                                :opt6   >some 74
                                                :set4   >(1KqirW
                                                :str2   >(6FAycW
                                                :str3   '190'

    *rga    #(6FAycW                @(4pX_g8    :0      !
                                    @)6                 '2'
                                    @)7                 '7'
                                    @)8                 '5'

    *set    #(1KqirW                @`                  !
    .
    |]

ex4expect :: ByteStringL
ex4expect = [i|
    *lww    #B/00009ISodW+r3pl1c4   @`(c1l2MW               !
                                    @(D81V2W    :int1       166
                                    @`          :opt5       >none
                                    @(c1l2MW    :opt6       >none
                                    @`          :set4       >(1KqirW
                                                :str2       >(6FAycW
                                    @(PEddUW    :str3       '206'

            #(YD2ZdW                @`          :0          !
                                                :int1       135
                                                :opt5       >none
                                                :opt6       >none
                                                :set4       >(T6VGUW
                                                :str2       >(XvcLcW
                                                :str3       '137'

    *rga    #(6FAycW                @(LxA_UW    :0          !
                                    @(4pX_g6    :`(E4W2MW   '2'
                                    @)7         :(HTTXUW    '7'
                                    @(LJJfUW    :0          '1'
                                    @[xA_UW                 '4'
                                    @(4pX_g8                '5'

            #(XvcLcW                @(X530g8                !
                                    @)6                     '1'
                                    @)7                     '3'
                                    @)8                     '6'

    *set    #(1KqirW                @(asoV2W                !
                                    @                       >(YD2ZdW

            #(T6VGUW                @`                      !
    .
    |]

example4expect :: Struct51
example4expect = Struct51
    { int1 = 166
    , str2 = RGA "145"
    , str3 = "206"
    , set4 = ORSet [Struct51
        { int1 = 135
        , str2 = RGA "136"
        , str3 = "137"
        , set4 = ORSet []
        , opt5 = Nothing
        , opt6 = Nothing
        }]
    , opt5 = Nothing
    , opt6 = Nothing
    }

prop_lwwStruct :: Property
prop_lwwStruct = property $ do
    -- create an object
    let ex1state =
            runNetworkSim $ runReplicaSim replica $ newObjectState example0
    let (oid, ex1ser) = serializeObject ex1state
    prep ex1expect === prep ex1ser

    -- parse newly created object
    ex2state <- evalEither $ parseObject oid ex1ser
    ex1state === ex2state

    -- decode newly created object
    example3 <- evalEither $ evalObjectState ex2state getObject
    example0 === example3

    -- apply operations to the object (frame)
    ex4state <-
        evalExceptT $
        runNetworkSimT $ runReplicaSimT replica $
        execObjectState ex2state $ do
            -- plain field
            int1_assign 166
            str2_zoom $ RGA.edit "145"
            str3Value <- str3_read
            str3Value === "190"
            str3_assign "206"
            set4_zoom $
                ORSet.addValue
                    Struct51
                        { int1 = 135
                        , str2 = RGA "136"
                        , str3 = "137"
                        , set4 = ORSet []
                        , opt5 = Nothing
                        , opt6 = Nothing
                        }
            opt5Value <- opt5_read
            opt5Value === Nothing
            opt6Value <- opt6_read
            opt6Value === Just 74
            opt6_assign Nothing

    -- decode object after modification
    example4 <- evalEither $ evalObjectState ex4state getObject
    example4expect === example4

    -- serialize object after modification
    parseObject oid ex4expect === Right ex4state
    prep ex4expect === prep (snd $ serializeObject ex4state)

  where
    prep = filter (not . null) . map BSLC.words . BSLC.lines
