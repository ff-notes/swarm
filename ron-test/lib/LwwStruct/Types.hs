{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LwwStruct.Types (
    Struct51 (..),
    int1_assign,
    int1_read,
    int1_zoom,
    str2_assign,
    str2_read,
    str2_zoom,
    str3_assign,
    str3_read,
    str3_zoom,
    set4_assign,
    set4_read,
    set4_zoom,
    opt5_assign,
    opt5_read,
    opt5_zoom,
    opt6_assign,
    opt6_read,
    opt6_zoom,
    -- * tests
    tfp_field_assign,
    tfp_field_read,
    tfp_field_zoom,
    tfpatInnerField_assign,
    tfpatInnerField_read,
    tfpatInnerField_zoom,
    Enum67 (..),
) where

import           RON.Prelude

import           Data.Type.Equality ((:~:) (Refl))

import           RON.Data (Replicated, ReplicatedAsPayload, encoding,
                           fromPayload, payloadEncoding, toPayload)
import           RON.Data.ORSet (ORSet, ORSetMap)
import           RON.Schema.TH (mkReplicated)

data Opaque49 = Opaque49
    deriving (Eq, Show)
instance Replicated Opaque49 where encoding = payloadEncoding
instance ReplicatedAsPayload Opaque49 where
    toPayload   = undefined
    fromPayload = undefined

[mkReplicated|
    (opaque atoms Opaque49)

    (struct_lww Struct51
        int1 Integer
        str2 RgaString
        str3 String
        set4 Alias69
        opt5 (Option Struct51)
        opt6 (Option Integer))

    (struct_lww TestFieldPrefix
        #haskell {field_prefix "tfp_"}
        field (Option Opaque49))

    (struct_lww TestFieldPrefixAndTitle
        #haskell {field_prefix "tfpat", field_case title}
        innerField Integer)

    (enum Enum67 E67Item1 E67Item2)

    (alias Alias69 (ORSet Struct51))

    (alias Alias75 (ORSet.Map String String))
|]

deriving instance Eq   Struct51
deriving instance Show Struct51

_check_Alias69 :: Alias69 :~: ORSet Struct51
_check_Alias69 = Refl

_check_Alias75 :: Alias75 :~: ORSetMap Text Text
_check_Alias75 = Refl
