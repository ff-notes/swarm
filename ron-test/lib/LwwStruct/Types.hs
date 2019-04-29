{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module LwwStruct.Types (
    Example51 (..),
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
    TestEnum67 (..),
) where

import           RON.Prelude

import           RON.Data (Replicated, ReplicatedAsPayload, encoding,
                           fromPayload, payloadEncoding, toPayload)
import           RON.Schema.TH (mkReplicated)

data TestOpaque = TestOpaque
    deriving (Eq, Show)
instance Replicated TestOpaque where encoding = payloadEncoding
instance ReplicatedAsPayload TestOpaque where
    toPayload   = undefined
    fromPayload = undefined

[mkReplicated|
    (opaque atoms TestOpaque)

    (struct_lww Example51
        int1 Integer
        str2 RgaString
        str3 String
        set4 TestAlias69
        opt5 (Option Example51)
        opt6 (Option Integer))

    (struct_lww TestFieldPrefix
        #haskell {field_prefix "tfp_"}
        field (Option TestOpaque))

    (struct_lww TestFieldPrefixAndTitle
        #haskell {field_prefix "tfpat", field_case title}
        innerField Integer)

    (enum TestEnum67 TEItem1 TEItem2)

    (alias TestAlias69 (ORSet Example51))
|]

deriving instance Eq   Example51
deriving instance Show Example51
