{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module LwwStruct.Types (
    Example1 (..),
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
    Example2 (..),
    vv5_assign,
    vv5_read,
    vv5_zoom,
    -- * tests
    tfp_field_assign,
    tfp_field_read,
    tfp_field_zoom,
    tfpatInnerField_assign,
    tfpatInnerField_read,
    tfpatInnerField_zoom,
) where

import           RON.Schema.TH (mkReplicated)

[mkReplicated|
    (struct_lww Example2
        vv5 VersionVector)

    (struct_lww Example1
        int1 Atom.Integer
        str2 RgaString
        str3 Atom.String
        set4 (ORSet Example2)
        opt5 (Option Example2)
        opt6 (Option Atom.Integer))

    (struct_lww TestFieldPrefix
        #haskell {field_prefix "tfp_"}
        field Atom.Integer)

    (struct_lww TestFieldPrefixAndTitle
        #haskell {field_prefix "tfpat", field_case title}
        innerField Atom.Integer)
|]

deriving instance Eq   Example1
deriving instance Show Example1

deriving instance Eq   Example2
deriving instance Show Example2
