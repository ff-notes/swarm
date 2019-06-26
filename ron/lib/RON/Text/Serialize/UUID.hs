{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module RON.Text.Serialize.UUID (
    serializeUuid,
    serializeUuidAtom,
    serializeUuidKey,
) where

import           RON.Prelude

import           Data.Bits (countLeadingZeros, shiftL, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (minimumBy)

import qualified RON.Base64 as Base64
import           RON.Util (ByteStringL)
import           RON.Util.Word (pattern B00, pattern B0000, pattern B01,
                                pattern B10, pattern B11, Word2, Word60, ls60,
                                safeCast)
import           RON.UUID (UUID (..), UuidFields (..), split, zero)

-- | Serialize UUID without context (used for test)
serializeUuid :: UUID -> ByteStringL
serializeUuid this =
    BSL.fromStrict $ case uuidVariant of
        B00 -> unzipped thisFields
        _   -> serializeUuidGeneric this
  where
    thisFields@UuidFields{..} = split this

-- | Serialize UUID in op key context
serializeUuidKey
    :: UUID  -- ^ same key in the previous op (default is 'zero')
    -> UUID  -- ^ previous key of the same op (default is 'zero')
    -> UUID  -- ^ this
    -> ByteStringL
serializeUuidKey prevKey prev this =
    BSL.fromStrict $ case uuidVariant thisFields of
        B00 -> minimumByLength
            $   unzipped thisFields
            :   [ z
                | uuidVariant (split prevKey) == B00
                , Just z <- [zipUuid (split prevKey) thisFields]
                ]
            ++  [ "`" <> z
                | prev /= zero
                , uuidVariant (split prev) == B00
                , Just z <- [zipUuid (split prev) thisFields]
                ]
        _   -> serializeUuidGeneric this
  where
    thisFields = split this

-- | Serialize UUID in op value (atom) context
serializeUuidAtom
    :: UUID  -- ^ previous
    -> UUID  -- ^ this
    -> ByteStringL
serializeUuidAtom prev this =
    BSL.fromStrict $ case uuidVariant thisFields of
        B00 -> minimumByLength
            $   unzipped thisFields
            :   [ z
                | prev /= zero
                , uuidVariant (split prev) == B00
                , Just z <- [zipUuid (split prev) thisFields]
                ]
        _   -> serializeUuidGeneric this
  where
    thisFields = split this

unzipped :: UuidFields -> ByteString
unzipped UuidFields{..} = x' <> y'
  where
    variety = case uuidVariety of
        B0000 -> ""
        _     -> BS.singleton (Base64.encodeLetter4 uuidVariety) <> "/"
    x' = variety <> Base64.encode60short uuidValue
    y' = case (uuidVersion, uuidOrigin) of
        (B00, safeCast -> 0 :: Word64) -> ""
        _ ->
            serializeVersion uuidVersion
            `BSC.cons` Base64.encode60short uuidOrigin

zipUuid :: UuidFields -> UuidFields -> Maybe ByteString
zipUuid prev this
    | prev == this  = Just ""
    | canReuseValue = valueZip
    | otherwise     = Nothing
  where
    canReuseValue = prev{uuidValue = uuidValue this} == this
    valueZip = zipPrefix (uuidValue prev) (uuidValue this)

zipPrefix :: Word60 -> Word60 -> Maybe ByteString
zipPrefix prev this
    | commonBits >= 6 * 10 = pure ""
    | commonBits >= 6 *  9 = ok ')' 9
    | commonBits >= 6 *  8 = ok ']' 8
    | commonBits >= 6 *  7 = ok '}' 7
    | commonBits >= 6 *  6 = ok '{' 6
    | commonBits >= 6 *  5 = ok '[' 5
    | commonBits >= 6 *  4 = ok '(' 4
    | otherwise        = Nothing
  where
    ok c n = pure $ BSC.cons c $ encode60short' $ safeCast this `shiftL` (6 * n)
    commonBits =
        countLeadingZeros (safeCast prev `xor` safeCast this :: Word64) - 4
    encode60short' = \case
        0 -> ""
        w -> Base64.encode60short $ ls60 w

serializeVersion :: Word2 -> Char
serializeVersion = \case
    B00 -> '$'
    B01 -> '%'
    B10 -> '+'
    B11 -> '-'

serializeUuidGeneric :: UUID -> ByteString
serializeUuidGeneric (UUID x y) = Base64.encode64 x <> Base64.encode64 y

-- | XXX Partial for lists!
minimumByLength :: Foldable f => f ByteString -> ByteString
minimumByLength = minimumBy $ comparing BS.length
