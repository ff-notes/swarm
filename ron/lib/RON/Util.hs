{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module RON.Util (
    ByteStringL,
    Instance (Instance),
    runStateAsWriter,
) where

import           RON.Prelude

import qualified Data.ByteString.Lazy as BSL

type ByteStringL = BSL.ByteString

data Instance c = forall a . c a => Instance a

runStateAsWriter :: Semigroup s => MonadState s m => WriterT s m a -> m a
runStateAsWriter action = do
    (a, s) <- runWriterT action
    modify' (s <>)
    pure a
