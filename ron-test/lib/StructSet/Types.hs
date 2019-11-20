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
        (int1 min   Integer)
        (str2 merge RgaString)
        (str3 lww   String)
        (set4 merge (ORSet StructSet13))
        (set5 set   Integer)
        (nst6 merge StructSet13)
        (ref7 set   (ObjectRef StructSet13)))
|]

deriving instance Default StructSet13
deriving instance Eq      StructSet13
deriving instance Generic StructSet13
deriving instance Show    StructSet13
