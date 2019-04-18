{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Prelude (
    module X,
    fmapL,
    foldr1,
    headMay,
    identity,
    lastDef,
    maximumDef,
    maxOn,
    minOn,
    note,
    replicate2,
    replicate3,
    show,
    whenJust,
    (!!),
    (?:),
) where

-- base
import           Control.Applicative as X (Alternative, Applicative, liftA2,
                                           many, optional, pure, some, (*>),
                                           (<*), (<*>), (<|>))
import           Control.Exception as X (Exception, catch, evaluate, throwIO)
import           Control.Monad as X (Monad, filterM, guard, unless, void, when,
                                     (<=<), (=<<), (>=>), (>>=))
import           Control.Monad.Fail as X (MonadFail, fail)
import           Control.Monad.IO.Class as X (MonadIO, liftIO)
import           Data.Bifunctor as X (bimap)
import           Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import           Data.Char as X (Char, chr, ord, toLower, toUpper)
import           Data.Coerce as X (Coercible, coerce)
import           Data.Data as X (Data)
import           Data.Either as X (Either (Left, Right), either)
import           Data.Eq as X (Eq, (/=), (==))
import           Data.Foldable as X (Foldable, and, asum, fold, foldMap, foldl',
                                     foldr, for_, length, minimumBy, null, or,
                                     toList, traverse_)
import           Data.Function as X (const, flip, on, ($), (.))
import           Data.Functor as X (Functor, fmap, ($>), (<$), (<$>))
import           Data.Functor.Identity as X (Identity)
import           Data.Int as X (Int, Int16, Int32, Int64, Int8)
import           Data.IORef as X (IORef, atomicModifyIORef', newIORef,
                                  readIORef, writeIORef)
import           Data.List as X (drop, filter, genericLength, intercalate,
                                 isPrefixOf, isSuffixOf, lookup, map, partition,
                                 repeat, replicate, sortBy, sortOn, span,
                                 splitAt, take, takeWhile, unlines, unwords,
                                 zip, (++))
import           Data.List.NonEmpty as X (NonEmpty ((:|)), nonEmpty)
import           Data.Maybe as X (Maybe (Just, Nothing), catMaybes, fromMaybe,
                                  listToMaybe, maybe, maybeToList)
import           Data.Monoid as X (Last (Last), Monoid, mempty)
import           Data.Ord as X (Down (Down), Ord, Ordering (EQ, GT, LT),
                                compare, comparing, max, min, (<), (<=), (>),
                                (>=))
import           Data.Ratio as X ((%))
import           Data.Semigroup as X (Semigroup, sconcat, (<>))
import           Data.String as X (String)
import           Data.Traversable as X (for, sequence, sequenceA, traverse)
import           Data.Tuple as X (fst, snd, uncurry)
import           Data.Typeable as X (Typeable)
import           Data.Word as X (Word, Word16, Word32, Word64, Word8)
import           GHC.Enum as X (Bounded, Enum, fromEnum, maxBound, minBound,
                                pred, succ, toEnum)
import           GHC.Err as X (error, undefined)
import           GHC.Exts as X (Double)
import           GHC.Generics as X (Generic)
import           GHC.Integer as X (Integer)
import           GHC.Num as X (Num, negate, subtract, (*), (+), (-))
import           GHC.Real as X (Integral, fromIntegral, mod, realToFrac, round,
                                (^), (^^))
import           GHC.Stack as X (HasCallStack)
import           System.IO as X (FilePath, IO)
import           Text.Show as X (Show)

#ifdef VERSION_bytestring
import           Data.ByteString as X (ByteString)
#endif

#ifdef VERSION_containers
import           Data.Map.Strict as X (Map)
#endif

#ifdef VERSION_deepseq
import           Control.DeepSeq as X (NFData, force)
#endif

#ifdef VERSION_filepath
import           System.FilePath as X ((</>))
#endif

#ifdef VERSION_hashable
import           Data.Hashable as X (Hashable, hash)
#endif

#ifdef VERSION_mtl
import           Control.Monad.Except as X (ExceptT, MonadError, catchError,
                                            liftEither, runExceptT, throwError)
import           Control.Monad.Reader as X (ReaderT (ReaderT), ask, reader,
                                            runReaderT)
import           Control.Monad.State.Strict as X (MonadState, State, StateT,
                                                  evalState, evalStateT,
                                                  execStateT, get, gets,
                                                  modify', put, runState,
                                                  runStateT, state)
import           Control.Monad.Trans as X (MonadTrans, lift)
import           Control.Monad.Writer.Strict as X (MonadWriter, WriterT,
                                                   runWriterT, tell)
#endif

#ifdef VERSION_text
import           Data.Text as X (Text)
#endif

#ifdef VERSION_time
import           Data.Time as X (UTCTime)
#endif

#ifdef VERSION_unordered_containers
import           Data.HashMap.Strict as X (HashMap)
#endif

--------------------------------------------------------------------------------

import qualified Data.Foldable
import           Data.List (last, maximum)
import           Data.String (IsString, fromString)
import qualified Text.Show

fmapL :: (a -> b) -> Either a c -> Either b c
fmapL f = either (Left . f) Right

foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 = Data.Foldable.foldr1

headMay :: [a] -> Maybe a
headMay = \case
    []  -> Nothing
    a:_ -> Just a

identity :: a -> a
identity x = x

lastDef :: a -> [a] -> a
lastDef def = list' def last

list' :: b -> ([a] -> b) -> [a] -> b
list' onEmpty onNonEmpty = \case
    [] -> onEmpty
    xs -> onNonEmpty xs

maximumDef :: Ord a => a -> [a] -> a
maximumDef def = list' def maximum

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x < f y then y else x

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y = if f x < f y then x else y

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

replicate2 :: Applicative m => m a -> m (a, a)
replicate2 ma = (,) <$> ma <*> ma

replicate3 :: Applicative m => m a -> m (a, a, a)
replicate3 ma = (,,) <$> ma <*> ma <*> ma

show :: (Show a, IsString s) => a -> s
show = fromString . Text.Show.show

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (pure ()) f m

(!!) :: [a] -> Int -> Maybe a
xs !! i
    | i < 0     = Nothing
    | otherwise = headMay $ drop i xs

-- | An infix form of 'fromMaybe' with arguments flipped.
(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINABLE (?:) #-}
infixr 0 ?:
