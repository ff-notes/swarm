{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | RON version of Base64 encoding
module RON.Base64
    ( alphabet
    , decode
    , decode60
    , decode60base32
    , decode64
    , decode64base32
    , decodeLetter
    , decodeLetter4
    , encode
    , encode60
    , encode60short
    , encode64
    , encode64base32short
    , encodeLetter
    , encodeLetter4
    , isLetter
    ) where

import           RON.Internal.Prelude

import           Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (ord)

import           RON.Internal.Word (Word4, Word6 (W6), Word60,
                                    leastSignificant4, leastSignificant6,
                                    leastSignificant60, safeCast)

-- | Base64 alphabet
alphabet :: ByteString
alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"

-- | Check if a character is in the Base64 alphabet.
isLetter :: Word8 -> Bool
isLetter c =    c - ord0 <= 9
             || c - ordA <= 25
             || c - orda <= 25
             || c == ord_
             || c == ordõ

-- | Convert a Base64 letter to a number [0-63]
decodeLetter :: Word8 -> Maybe Word6
decodeLetter x
    | x <  ord0 = Nothing
    | x <= ord9 = Just . leastSignificant6 $ x - ord0
    | x <  ordA = Nothing
    | x <= ordZ = Just . leastSignificant6 $ x - ordA + posA
    | x == ord_ = Just $ leastSignificant6 pos_
    | x <  orda = Nothing
    | x <= ordz = Just . leastSignificant6 $ x - orda + posa
    | x == ordõ = Just $ leastSignificant6 posõ
    | otherwise  = Nothing

-- ASCII milestone codes
ord0, ord9, ordA, ordZ, ord_, orda, ordz, ordõ :: Word8
ord0 = fromIntegral $ ord '0'
ord9 = fromIntegral $ ord '9'
ordA = fromIntegral $ ord 'A'
ordZ = fromIntegral $ ord 'Z'
ord_ = fromIntegral $ ord '_'
orda = fromIntegral $ ord 'a'
ordz = fromIntegral $ ord 'z'
ordõ = fromIntegral $ ord '~'

-- Base64 milestone codes
posA, pos_, posa, posõ :: Word8
posA = 10
pos_ = 36
posa = 37
posõ = 63

-- | Convert a subset [0-F] of Base64 letters to a number [0-15]
decodeLetter4 :: Word8 -> Maybe Word4
decodeLetter4 x
    | x <  ord0 = Nothing
    | x <= ord9 = Just . leastSignificant4 $ x - ord0
    | x <  ordA = Nothing
    | x <= ordZ = Just . leastSignificant4 $ x - ordA + posA
    | otherwise  = Nothing

-- | Decode a blob from a Base64 string
decode :: ByteStringL -> Maybe ByteStringL
decode =
    fmap (BSL.pack . go . map safeCast) . traverse decodeLetter . BSL.unpack
  where
    go = \case
        [a, b]       -> decode2 a b
        [a, b, c]    -> decode3 a b c
        a:b:c:d:rest -> decode4 a b c d ++ go rest
        _            -> []
    decode2 a b = [(a `shiftL` 2) .|. (b `shiftR` 4)]
    decode3 a b c =
        [ ( a             `shiftL` 2) .|. (b `shiftR` 4)
        , ((b .&. 0b1111) `shiftL` 4) .|. (c `shiftR` 2)
        ]
    decode4 a b c d =
        [ ( a             `shiftL` 2) .|. (b `shiftR` 4)
        , ((b .&. 0b1111) `shiftL` 4) .|. (c `shiftR` 2)
        , ((c .&.   0b11) `shiftL` 6) .|.  d
        ]

-- | Decode a 60-bit number from a Base64 string
decode60 :: ByteString -> Maybe Word60
decode60 =
    fmap leastSignificant60 . go 10
    <=< traverse (fmap safeCast . decodeLetter) . BS.unpack
  where
    go :: Int -> [Word8] -> Maybe Word64
    go n
        | n > 0 = \case
            []           -> Just 0
            [a]          -> Just $ decode4 a 0 0 0
            [a, b]       -> Just $ decode4 a b 0 0
            [a, b, c]    -> Just $ decode4 a b c 0
            a:b:c:d:rest -> do
                lowerPart <- go (n - 4) rest
                pure $ decode4 a b c d .|. (lowerPart `shiftR` 24)
        | otherwise = \case
            [] -> Just 0
            _  -> Nothing  -- extra input
    decode4 :: Word8 -> Word8 -> Word8 -> Word8 -> Word64
    decode4 a b c d =
        (safeCast a `shiftL` 54) .|.
        (safeCast b `shiftL` 48) .|.
        (safeCast c `shiftL` 42) .|.
        (safeCast d `shiftL` 36)

-- | Decode a 60-bit number from a Base32 string
decode60base32 :: ByteString -> Maybe Word60
decode60base32 =
    fmap leastSignificant60 . go12
    <=< traverse (fmap safeCast . decodeLetter) . BS.unpack
  where
    go12 :: [Word8] -> Maybe Word64
    go12 letters = do
        let (letters8, letters4) = splitAt 8 letters
            w8 = decodeBase32 8 letters8
        w4 <- go4 letters4
        pure $ (w8 `shiftL` 20) .|. w4
    go4 :: [Word8] -> Maybe Word64
    go4 letters = case splitAt 4 letters of
        (letters4, []) -> pure $ decodeBase32 4 letters4
        _ -> Nothing  -- extra input
    decodeBase32 :: Int -> [Word8] -> Word64
    decodeBase32 len
        = foldl' (\acc b -> (acc `shiftL` 5) .|. safeCast b) 0
        . take len
        . (++ repeat 0)

-- | Decode a 64-bit number from a Base64 string
decode64 :: ByteString -> Maybe Word64
decode64 s = do
    (s0, s1) <- BS.uncons s
    cons64 <$> decodeLetter4 s0 <*> decode60 s1

-- | Decode a 64-bit number from a Base32 string
decode64base32 :: ByteString -> Maybe Word64
decode64base32 s = do
    (s0, s1) <- BS.uncons s
    cons64 <$> decodeLetter4 s0 <*> decode60base32 s1

cons64 :: Word4 -> Word60 -> Word64
cons64 v w = (safeCast v `shiftL` 60) .|. safeCast w

-- | Encode a blob to a Base64 string
encode :: ByteStringL -> ByteStringL
encode = BSL.pack . go . BSL.unpack
  where
    go = \case
        []         -> []
        [a]        -> encode1 a
        [a, b]     -> encode2 a b
        a:b:c:rest -> encode3 a b c ++ go rest
    encode1 a =
        map (encodeLetter . leastSignificant6)
            [a `shiftR` 2, (a .&. 0b11) `shiftL` 4]
    encode2 a b = map (encodeLetter . leastSignificant6)
        [                                  a `shiftR` 2
        , ((a .&.   0b11) `shiftL` 4) .|. (b `shiftR` 4)
        ,  (b .&. 0b1111) `shiftL` 2
        ]
    encode3 a b c = map (encodeLetter . leastSignificant6)
        [                                    a `shiftR` 2
        , ((a .&.     0b11) `shiftL` 4) .|. (b `shiftR` 4)
        , ((b .&.   0b1111) `shiftL` 2) .|. (c `shiftR` 6)
        ,   c .&. 0b111111
        ]

-- | Convert a number from [0..63] to a single letter
encodeLetter :: Word6 -> Word8
encodeLetter i = alphabet `BS.index` safeCast i

-- | Convert a number from [0..15] to a single letter
encodeLetter4 :: Word4 -> Word8
encodeLetter4 i = alphabet `BS.index` safeCast i

-- | Encode a 60-bit number to a Base64 string
encode60 :: Word60 -> ByteString
encode60 w = BS.pack $
    map (encodeLetter . leastSignificant6)
        [ (safeCast w `shiftR` (6 * i)) .&. 0b111111 :: Word64
        | i <- [9, 8 .. 0]
        ]

-- | Encode a 60-bit number to a Base64 string, dropping trailing zeroes
encode60short :: Word60 -> ByteString
encode60short v = case safeCast v :: Word64 of
    0 -> "0"
    x -> BS.pack . map (encodeLetter . leastSignificant6) $ go 9 x
  where
    go _ 0 = []
    go i w =
        (w `shiftR` (6 * i)) .&. 0b111111 :
        go (i - 1) (w .&. complement (0b111111 `shiftL` (6 * i)))

-- | Encode a 64-bit number to a Base32 string, dropping trailing zeroes
encode64base32short :: Word64 -> ByteString
encode64base32short = \case
    0 -> "0"
    x -> BS.pack . map (encodeLetter . leastSignificant5) $ go 12 x
  where
    go _ 0 = []
    go i w =
        (w `shiftR` (5 * i)) .&. 0b11111 :
        go (i - 1) (w .&. complement (0b11111 `shiftL` (5 * i)))

    leastSignificant5 w = W6 $ fromIntegral w .&. 0b11111

-- | Encode a 64-bit number to a Base64 string
encode64 :: Word64 -> ByteString
encode64 w =
    encodeLetter (leastSignificant6 $ w `shiftR` 60)
    `BS.cons` encode60 (leastSignificant60 w)
