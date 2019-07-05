{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module RON.Util (
    ByteStringL,
    Instance (Instance),
) where

import qualified Data.ByteString.Lazy as BSL

type ByteStringL = BSL.ByteString

data Instance c = forall a . c a => Instance a
