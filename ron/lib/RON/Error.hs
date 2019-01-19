{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module RON.Error (
    Error (..),
    MonadE,
    errorContext,
    liftMaybe,
    throwErrorString,
    throwErrorText,
) where

import           Data.String (IsString, fromString)

data Error
    = Error        Text [Error]
    | ErrorContext Text  Error
    deriving (Exception, Eq, Show)

instance IsString Error where
    fromString s = Error (fromString s) []

type MonadE = MonadError Error

errorContext :: MonadE m => Text -> m a -> m a
errorContext ctx action = action `catchError` (throwError . ErrorContext ctx)

liftMaybe :: MonadE m => Text -> Maybe a -> m a
liftMaybe msg = maybe (throwErrorText msg) pure

throwErrorText :: MonadE m => Text -> m a
throwErrorText msg = throwError $ Error msg []

throwErrorString :: (MonadError e m, IsString e) => String -> m a
throwErrorString msg = throwError $ fromString msg
