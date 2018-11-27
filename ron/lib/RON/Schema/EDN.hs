{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Schema.EDN (parseSchema) where

import           RON.Internal.Prelude

import           Control.Monad.State.Strict (StateT, evalStateT, get, gets, put)
import           Control.Monad.Trans (MonadTrans, lift)
import           Control.Monad.Trans.Identity (runIdentityT)
import           Data.Attoparsec.Lazy (Result (Done))
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (isSpace)
import           Data.EDN (Tagged (NoTag, Tagged), Value (List, Map, Symbol),
                           (.!=), (.:?))
import           Data.EDN.Parser (parseBSL)
import           Data.EDN.Types (EDNList, EDNMap)
import           Data.EDN.Types.Class (Parser, parseEither, typeMismatch)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL

import           RON.Schema

newtype Env = Env {knownTypes :: Map Text RonType}
    deriving (Show)

startEnv :: Env
startEnv = Env
    { knownTypes = Map.fromList
        [ ("Atom.Integer",  atomInteger)
        , ("Atom.String",   atomString)
        , ("RgaString",     rgaString)
        , ("VersionVector", versionVector)
        ]
    }

type Parser' = StateT Env Parser

parseDeclaration :: Tagged Value -> Parser' Declaration
parseDeclaration = withNoTag $ withList "declaration" $ \case
    func : args -> do
        func' <- withNoTag pure func
        withSymbol "declaration name symbol" (go args) func'
    [] -> fail "empty declaration"
  where
    go args = withNoPrefix "declaration name" $ \case
        "struct_lww" -> DStructLww <$> parseStructLww args
        name         -> fail $ "unknown declaration " ++ decodeUtf8 name

parseStructLww :: EDNList -> Parser' StructLww
parseStructLww code = do
    struct@StructLww{structName} <- case code of
        name : body -> do
            let (annotations, fields) = span isTagged body
            StructLww
                <$> parseName name
                <*> parseFields fields
                <*> parseAnnotations annotations
        _ -> fail
            "Expected declaration in the form\
            \ (struct_lww <name:symbol> <fields:map> <annotations>...)"
    env@Env{knownTypes} <- get
    case Map.lookup structName knownTypes of
        Nothing ->
            put env { knownTypes =
                        Map.insert structName (structLww struct) knownTypes
                    }
        Just _ ->
            fail $ "duplicate declaration of type " ++ Text.unpack structName
    pure struct

  where

    parseName =
        withNoTag $
        withSymbol "struct_lww name symbol" $
        withNoPrefix "struct_lww name" $
        pure . Text.decodeUtf8

    parseFields = \case
        [] -> pure mempty
        nameTV : typeTV : cont -> do
            name <- parseFieldName nameTV
            typ  <- parseType typeTV
            Map.insert name (Field typ FieldAnnotations) <$> parseFields cont
        [_] -> fail "field format error"
      where
        parseFieldName =
            withNoTag $
            withSymbol "struct_lww field name symbol" $
            withNoPrefix "struct_lww field name" $
            pure . Text.decodeUtf8

    parseAnnotations annTaggedValues = do
        annValues <- traverse unwrapTag annTaggedValues
        case lookup "/haskell" annValues of
            Nothing -> pure def
            Just annValue ->
                withMap "struct_lww haskell annotations map" go annValue
      where
        unwrapTag = \case
            Tagged value prefix tag -> pure (prefix <> "/" <> tag, value)
            NoTag _ -> fail "annotation must be a tagged value"
        go m = lift $
            StructAnnotations
            <$>  m .:? Symbol "" "field_prefix" .!= ""
            <*> (m .:? Symbol "" "field_case" >>= traverse parseCaseTransform)

parseCaseTransform :: Value -> Parser CaseTransform
parseCaseTransform =
    runIdentityT .
    withSymbol "case transformation symbol"
        (withNoPrefix "case transformation" go)
  where
    go = \case
        "title" -> pure TitleCase
        _       -> fail "unknown case transformation"

parseSchema :: Monad m => String -> m Schema
parseSchema string = either fail pure $ do
    values <- parseEdnStream $ encodeUtf8L string
    parseEither ((`evalStateT` startEnv) . traverse parseDeclaration) values

parseEdnStream :: ByteStringL -> Either String EDNList
parseEdnStream input
    | BSLC.all isSpace input = pure []
    | otherwise     =
        case parseBSL input of
            Done rest value -> (value :) <$> parseEdnStream rest
            failure         -> Left $ show failure

parseType :: Tagged Value -> Parser' RonType
parseType = withNoTag $ \case
    Symbol "" typeName -> case typeName of
        name            ->
            gets (Map.lookup (Text.decodeUtf8 name) . knownTypes) >>= \case
                Nothing  -> fail $ "unknown type " ++ decodeUtf8 name
                Just typ -> pure typ
    Symbol _ _ -> fail "types must not be prefixed"
    List expr -> evalType expr
    value -> lift $ typeMismatch "type symbol or expression" value

evalType :: EDNList -> Parser' RonType
evalType = \case
    []  -> fail "empty type expression"
    [a] -> parseType a
    func : args -> applyType func =<< traverse parseType args

applyType :: Tagged Value -> [RonType] -> Parser' RonType
applyType func args =
    withNoTag
        (withSymbol "parametric type symbol" $
            withNoPrefix "parametric type" go)
        func
  where
    go = \case
        "Option" -> case args of
            [a] -> pure $ option a
            _  -> fail $ "Option expects 1 argument, got " ++ show (length args)
        "ORSet" -> case args of
            [a] -> pure $ orSet a
            _  -> fail $ "ORSet expects 1 argument, got " ++ show (length args)
        name -> fail $ "unknown parametric type " ++ decodeUtf8 name

-- * Parser helpers

withNoPrefix
    :: Monad m
    => String -> (ByteString -> m a) -> ByteString -> ByteString -> m a
withNoPrefix ctx f prefix name = do
    unless (prefix == "") $ fail $ ctx ++ ": empty prefix expected"
    f name

withList :: String -> (EDNList -> Parser' a) -> Value -> Parser' a
withList expected f = \case
    List list -> f list
    value     -> lift $ typeMismatch expected value

withMap :: String -> (EDNMap -> Parser' a) -> Value -> Parser' a
withMap expected f = \case
    Map m -> f m
    value -> lift $ typeMismatch expected value

withNoTag :: Monad m => (Value -> m a) -> Tagged Value -> m a
withNoTag f = \case
    NoTag value         -> f value
    Tagged _ prefix tag -> fail
        $ "when expecting a non-tagged value, encountered tag "
        ++ decodeUtf8 prefix ++ "/" ++ decodeUtf8 tag ++ " instead"

-- withString :: String -> (Text -> Parser a) -> Value -> Parser a
-- withString expected f = \case
--     String string -> f string
--     value         -> typeMismatch expected value

withSymbol
    :: MonadTrans t
    => String -> (ByteString -> ByteString -> t Parser a) -> Value -> t Parser a
withSymbol expected f = \case
    Symbol prefix symbol -> f prefix symbol
    value                -> lift $ typeMismatch expected value

-- withVector :: String -> (EDNVec -> Parser a) -> Value -> Parser a
-- withVector expected f = \case
--     Vec vec -> f vec
--     value   -> typeMismatch expected value

-- * ByteString helpers

decodeUtf8 :: ByteString -> String
decodeUtf8 = Text.unpack . Text.decodeUtf8

encodeUtf8L :: String -> ByteStringL
encodeUtf8L = TextL.encodeUtf8 . TextL.pack

isTagged :: Tagged a -> Bool
isTagged = \case
    NoTag {} -> False
    Tagged{} -> True
