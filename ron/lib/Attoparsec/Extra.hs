{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Attoparsec.Extra
    ( module Attoparsec
    , char
    , endOfInputEx
    , isSuccessful
    , label
    , parseOnlyL
    , takeL
    , definiteDouble
    , withInputSize
    , (??)
    , (<+>)
    ) where

import           RON.Prelude

import           Data.Attoparsec.ByteString.Char8 (anyChar, decimal, isDigit, isDigit_w8, signed)
import qualified Data.Attoparsec.Internal.Types as Internal
import           Data.Attoparsec.Lazy as Attoparsec
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Scientific as Sci
import           GHC.Real (toInteger)

import           RON.Util (ByteStringL)

parseOnlyL :: Parser a -> ByteStringL -> Either String a
parseOnlyL p = parseOnly p . toStrict

-- | 'Attoparsec.take' adapter to 'ByteStringL'
takeL :: Int -> Parser ByteStringL
takeL = fmap fromStrict . Attoparsec.take

getPos :: Parser Int
getPos =
    Internal.Parser $ \t pos more _ suc -> suc t pos more $ Internal.fromPos pos

withInputSize :: Parser a -> Parser (Int, a)
withInputSize p = do
    posBefore <- getPos
    r <- p
    posAfter <- getPos
    pure (posAfter - posBefore, r)

label :: String -> Parser a -> Parser a
label = flip (<?>)

-- label' :: String -> Parser a -> Parser a
-- label' name p = do
--     pos <- getPos
--     label (name ++ ':' : show pos) p

-- | Variant of 'endOfInput' with a more debuggable message.
endOfInputEx :: Parser ()
endOfInputEx = do
    weAreAtEnd <- atEnd
    unless weAreAtEnd $ do
        pos <- getPos
        rest <- takeAtMost 11
        let cite
                | BS.length rest < 11 = rest
                | otherwise           = BS.take 10 rest <> "..."
        fail $ show pos <> ": extra input: " <> show cite

takeAtMost :: Int -> Parser ByteString
takeAtMost limit = do
    pos0 <- getPos
    BS.pack <$> manyTill anyWord8 (checkLimit $ pos0 + limit)
  where
    checkLimit maxPos = do
        pos <- getPos
        guard (pos >= maxPos) <|> endOfInput

(??) :: Maybe a -> Parser a -> Parser a
(??) a alt = maybe alt pure a

-- | Apply parser and check it is applied successfully.
-- Kinda opposite to 'guard'.
isSuccessful :: Alternative f => f a -> f Bool
isSuccessful p = p $> True <|> pure False

char :: Char -> Parser Char
char c = do
    c' <- anyChar
    if c == c' then
        pure c
    else
        fail $ "Expected " ++ show c ++ ", got " ++ show c'

-- | Parses a definite double, i.e. it is not an integer. For this, the double has either a '.', and 'e'/'E' part or both.
{-# INLINE definiteDouble #-}
definiteDouble :: Parser Double
definiteDouble = do
    let parseIntegerPart = signed decimal
    let parseDot = char '.'
    let parseFractionalPartWithLength =
            BS.foldl' step (0, 0) `fmap` Attoparsec.takeWhile1 isDigit_w8
                where step (a, l) w = (a * 10 + fromIntegral (w - 48), l + 1)
    let parseExponent = (char 'e' <|> char 'E') *> signed decimal

    let withDot = do
          i <- optional parseIntegerPart
          parseDot
          (f, l) <- parseFractionalPartWithLength
          e <- optional parseExponent
          pure $ buildDouble (fromMaybe 0 i) f l (fromMaybe 0 e)

    let withE = do
          i <- optional parseIntegerPart
          e <- parseExponent
          pure $ buildDouble (fromMaybe 0 i) 0 0 e

    withDot <|> withE

buildDouble :: Integer -> Integer -> Int -> Int -> Double
buildDouble integerPart fractionalPart fractionalPartLength exponentPart =
    let addOrSubFractionalPart = if integerPart < 0 then -fractionalPart else fractionalPart
        coeff = integerPart * (10 ^ (toInteger fractionalPartLength)) + (toInteger addOrSubFractionalPart)
        exponent = exponentPart - fractionalPartLength
     in Sci.toRealFloat $ Sci.scientific coeff exponent

(<+>) :: Parser a -> Parser a -> Parser a
(<+>) p1 p2 = Internal.Parser $ \t pos more lose suc -> let
    lose1 t' _pos more1 ctx1 msg1 = Internal.runParser p2 t' pos more1 lose2 suc
      where
        lose2 _t _pos _more ctx2 msg2 = lose t pos more [] $ unwords
            [ "Many fails:\n"
            , intercalate " > " ctx1, ":", msg1, "|\n"
            , intercalate " > " ctx2, ":", msg2
            ]
    in Internal.runParser p1 t pos more lose1 suc
infixl 3 <+>
