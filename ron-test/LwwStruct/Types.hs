{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

module LwwStruct.Types where

import           RON.Schema.TH (mkReplicated)

[mkReplicated|
    (struct_lww Example2
        #haskell {field_prefix "example2_"}
        vv5 VersionVector)
    (struct_lww Example1
        #haskell {field_prefix "example1", field_case title}
        int1 Atom.Integer
        str2 RgaString
        str3 Atom.String
        set4 (ORSet Example2)
        opt5 (Option Example2)
        opt6 (Option Atom.Integer))
|]

deriving instance Eq   Example1
deriving instance Show Example1

deriving instance Eq   Example2
deriving instance Show Example2
