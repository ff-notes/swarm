{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module StructSet.Types where

import           RON.Prelude

import           Data.Default (Default)
import           RON.Schema.TH (mkReplicated)

[mkReplicated|
    (struct_set StructSet13
        int1 Integer
        str2 RgaString
        str3 String
        set4 (ORSet StructSet13)
        opt5 (Option StructSet13)
        nst6 StructSet13)
|]

deriving instance Default StructSet13
deriving instance Eq      StructSet13
deriving instance Generic StructSet13
deriving instance Show    StructSet13
