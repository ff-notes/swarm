{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Schema.EDN (readSchema) where

import           Prelude hiding (fail)

import           Control.Applicative (optional, (<|>))
import           Control.Monad (unless)
import           Control.Monad.Fail (MonadFail, fail)
import           Control.Monad.State.Strict (MonadState, execStateT, get, put)
import           Data.EDN (FromEDN, Tagged (NoTag, Tagged),
                           Value (List, Symbol), mapGetSymbol, parseEDN,
                           renderText, unexpected, withList, withMap, withNoTag)
import           Data.EDN.Class.Parser (parseM)
import           Data.EDN.Extra (decodeMultiDoc, isTagged, parseList,
                                 parseSymbol', withNoPrefix, withSymbol')
import           Data.Foldable (for_, traverse_)
import           Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text

import           RON.Schema

readSchema :: MonadFail m => String -> Text -> m (Schema 'Resolved)
readSchema sourceName source = do
    parsed <- parseSchema sourceName source
    env <- (`execStateT` Env{userTypes=Map.empty}) $ do
        collectDeclarations parsed
        validateTypeUses    parsed
    pure $ evalSchema env

newtype Env = Env{userTypes :: Map TypeName (Declaration 'Parsed)}
    deriving (Show)

data RonTypeF = Type0 RonType | Type1 (RonType -> RonType)

prelude :: Map TypeName RonTypeF
prelude = Map.fromList
    [ ("Boole",
        Type0 $
        opaqueAtoms "Boole" OpaqueAnnotations{oaHaskellType = Just "Bool"})
    , ("Day",           Type0 day)
    , ("Integer",       Type0 $ TAtom TAInteger)
    , ("RgaString",     Type0 $ TObject $ TRga char)
    , ("String",        Type0 $ TAtom TAString)
    , ("VersionVector", Type0 $ TObject TVersionVector)
    , ("Option",        Type1 $ TComposite . TOption)
    , ("ORSet",         Type1 $ TObject . TORSet)
    ]
  where
    char = opaqueAtoms "Char" OpaqueAnnotations{oaHaskellType = Just "Char"}
    day = opaqueAtoms_ "Day"

instance FromEDN (Declaration 'Parsed) where
    parseEDN = withNoTag . withList $ \case
        func : args -> (`withSymbol'` func) $ \case
            "enum"       -> DEnum      <$> parseList args
            "opaque"     -> DOpaque    <$> parseList args
            "struct_lww" -> DStructLww <$> parseList args
            name         -> fail $ "unknown declaration " ++ Text.unpack name
        [] -> fail "empty declaration"

instance FromEDN TEnum where
    parseEDN = withNoTag . withList $ \case
        name : items -> Enum
            <$> parseSymbol' name
            <*> traverse parseSymbol' items
        [] -> fail
            "Expected declaration in the form\
            \ (enum <name:symbol> <item:symbol>...)"

instance FromEDN Opaque where
    parseEDN = withNoTag . withList $ \case
        kind : name : annotations ->
            (`withSymbol'` kind) $ \case
                "atoms"  -> go False
                "object" -> go True
                _        -> fail "opaque kind must be either atoms or object"
            where
            go isObject =
                Opaque isObject <$> parseSymbol' name <*> parseAnnotations
            parseAnnotations = case annotations of
                [] -> pure defaultOpaqueAnnotations
                _  -> fail "opaque annotations are not implemented yet"
        _ -> fail
            "Expected declaration in the form\
            \ (opaque <kind:symbol> <name:symbol> <annotations>...)"

rememberDeclaration
    :: (MonadFail m, MonadState Env m) => Declaration 'Parsed -> m ()
rememberDeclaration decl = do
    env@Env{userTypes} <- get
    if name `Map.member` userTypes then
        fail $ "duplicate declaration of type " ++ Text.unpack name
    else
        put env {userTypes = Map.insert name decl userTypes}
  where
    name = declarationName decl

declarationName :: Declaration stage -> TypeName
declarationName = \case
    DEnum      Enum     {enumName  } -> enumName
    DOpaque    Opaque   {opaqueName} -> opaqueName
    DStructLww StructLww{structName} -> structName

instance FromEDN (StructLww 'Parsed) where
    parseEDN = withNoTag . withList $ \case
        name : body -> do
            let (annotations, fields) = span isTagged body
            StructLww
                <$> parseSymbol' name
                <*> parseFields fields
                <*> parseList annotations
        [] -> fail
            "Expected declaration in the form\
            \ (struct_lww <name:symbol> <annotations>... <fields>...)"

      where

        parseFields = \case
            [] -> pure mempty
            nameAsTagged : typeAsTagged : cont -> do
                name <- parseSymbol' nameAsTagged
                typ  <- parseEDN typeAsTagged
                Map.insert name (Field typ) <$> parseFields cont
            [f] ->
                fail $ "field " ++ Text.unpack (renderText f) ++ " must have type"

instance FromEDN StructAnnotations where
    parseEDN = withNoTag . withList $ \annTaggedValues -> do
        annValues <- traverse unwrapTag annTaggedValues
        case lookup "haskell" annValues of
            Nothing -> pure defaultStructAnnotations
            Just annValue -> withMap go annValue
      where
        unwrapTag = \case
            Tagged prefix tag value -> let
                name = case prefix of
                    "" -> tag
                    _  -> prefix <> "/" <> tag
                in pure (name, value)
            NoTag _ -> fail "annotation must be a tagged value"
        go m = do
            saHaskellFieldPrefix <- mapGetSymbol "field_prefix" m <|> pure ""
            saHaskellFieldCaseTransform <-
                optional $ mapGetSymbol "field_case" m
            pure StructAnnotations{..}

instance FromEDN CaseTransform where
    parseEDN = withSymbol' $ \case
        "title" -> pure TitleCase
        _       -> fail "unknown case transformation"

parseSchema :: MonadFail m => String -> Text -> m (Schema 'Parsed)
parseSchema sourceName source = do
    values <- decodeMultiDoc sourceName source
    parseM (traverse parseEDN) values

instance FromEDN TypeExpr where
    parseEDN = withNoTag $ \case
        Symbol prefix name -> withNoPrefix (pure . Use) prefix name
        List values -> do
            exprs <- traverse parseEDN values
            case exprs of
                []       -> fail "empty type expression"
                f : args -> case f of
                    Use typ -> pure $ Apply typ args
                    Apply{} ->
                        fail "type function must be a name, not expression"
        value -> value `unexpected` "type symbol or expression"

collectDeclarations :: (MonadFail m, MonadState Env m) => Schema 'Parsed -> m ()
collectDeclarations = traverse_ rememberDeclaration

validateTypeUses :: (MonadFail m, MonadState Env m) => Schema 'Parsed -> m ()
validateTypeUses = traverse_ $ \case
    DEnum      _                       -> pure ()
    DOpaque    _                       -> pure ()
    DStructLww StructLww{structFields} ->
        for_ structFields $ \(Field typeExpr) -> validateExpr typeExpr
  where
    validateName name = do
        Env{userTypes} <- get
        unless
            (name `Map.member` userTypes || name `Map.member` prelude)
            (fail $ "unknown type name " ++ Text.unpack name)
    validateExpr = \case
        Use name -> validateName name
        Apply name args -> do
            validateName name
            for_ args validateExpr

evalSchema :: Env -> Schema 'Resolved
evalSchema env = fst <$> userTypes' where
    Env{userTypes} = env
    userTypes' = evalDeclaration <$> userTypes

    evalDeclaration :: Declaration 'Parsed -> (Declaration 'Resolved, RonTypeF)
    evalDeclaration = \case
        DEnum   t -> (DEnum t, Type0 $ TComposite $ TEnum t)
        DOpaque t -> (DOpaque t, Type0 $ TOpaque t)
        DStructLww StructLww{..} -> let
            structFields' =
                (\(Field typeExpr) -> Field $ evalType typeExpr)
                <$> structFields
            struct = StructLww{structFields = structFields', ..}
            in (DStructLww struct, Type0 $ TObject $ TStructLww struct)

    getType typ = fromMaybe (snd $ userTypes' ! typ) $ prelude !? typ

    evalType = \case
        Use   typ      -> case getType typ of
            Type0 t0 -> t0
            Type1 _  -> error "type arity mismatch"
        Apply typ args -> applyType typ $ evalType <$> args

    applyType name args = case getType name of
        Type0 _  -> error "type arity mismatch"
        Type1 t1 -> case args of
            [a] -> t1 a
            _   -> error
                $   Text.unpack name ++ " expects 1 argument, got "
                ++  show (length args)
