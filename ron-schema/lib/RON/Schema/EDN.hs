{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-}

module RON.Schema.EDN
  ( readSchema,
  )
where

import Control.Arrow ((&&&))
import Data.EDN
  ( FromEDN,
    Tagged (NoTag, Tagged),
    TaggedValue,
    Value (List, Symbol),
    mapGetSymbol,
    parseEDN,
    -- renderText,
    unexpected,
    withList,
    withMap,
    withNoTag,
  )
import Data.EDN.Class.Parser (Parser, parseM)
import Data.EDN.Extra
  ( decodeMultiDoc,
    isTagged,
    parseList,
    parseSymbol',
    withNoPrefix,
    withSymbol',
  )
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import RON.Prelude
import RON.Schema

readSchema :: MonadFail m => String -> Text -> m (Schema 'Resolved)
readSchema sourceName source = do
  parsed <- parseSchema sourceName source
  env <- (`execStateT` Env{userTypes = Map.empty}) $ collectDeclarations parsed
  validateParsed env parsed
  let resolved = evalSchema env
  validateResolved resolved
  pure resolved

newtype Env = Env{userTypes :: Map TypeName (Declaration 'Parsed)}
  deriving (Show)

data RonTypeF
  = Type0 RonType
  | Type1 (RonType -> RonType)
  | Type2 (RonType -> RonType -> RonType)

prelude :: Map TypeName RonTypeF
prelude =
  Map.fromList
    [ ( "Bool",
        Type0 $ opaqueAtoms "Bool" OpaqueAnnotations{haskellType = Just "Bool"}
      ),
      ("Day", Type0 day),
      ("Float", Type0 $ TAtom TAFloat),
      ("Integer", Type0 $ TAtom TAInteger),
      ("ObjectRef", Type1 $ TAtom . TObjectRef),
      ("ORSet.Map", Type2 $ \k v -> TObject $ TORSetMap k v),
      ("ORSet", Type1 $ TObject . TORSet),
      ("RGA", Type1 $ TObject . TRga),
      ("RgaString", Type0 $ TObject $ TRga char),
      ("String", Type0 $ TAtom TAString),
      ("UUID", Type0 $ TAtom TAUuid),
      ("VersionVector", Type0 $ TObject TVersionVector)
    ]
  where
    char = opaqueAtoms "Char" OpaqueAnnotations{haskellType = Just "Char"}
    day = opaqueAtoms_ "Day"

instance FromEDN (Declaration 'Parsed) where
  parseEDN = withNoTag . withList $ \case
    func : args -> (`withSymbol'` func) $ \case
      "alias" -> DAlias <$> parseList args
      "enum" -> DEnum <$> parseList args
      "opaque_atoms" -> DOpaqueAtoms <$> parseList args
      "opaque_object" -> DOpaqueObject <$> parseList args
      "struct_lww" -> DStructLww <$> parseList args
      "struct_set" -> DStructSet <$> parseList args
      name -> fail $ "unknown declaration " ++ Text.unpack name
    [] -> fail "empty declaration"

instance FromEDN TEnum where
  parseEDN = withNoTag . withList $ \case
    nameSym : itemSyms -> do
      name <- parseSymbol' nameSym
      items <- traverse parseSymbol' itemSyms
      pure Enum{name, items}
    [] ->
      fail
        "Expected declaration in the form\
        \ (enum <name:symbol> <item:symbol>...)"

instance FromEDN Opaque where
  parseEDN = withNoTag . withList $ \case
    nameSym : annotationVals -> do
      name <- parseSymbol' nameSym
      annotations <- parseAnnotations
      pure Opaque{name, annotations}
      where
        parseAnnotations = case annotationVals of
          [] -> pure defaultOpaqueAnnotations
          _ -> fail "opaque annotations are not implemented yet"
    _ ->
      fail
        "Expected declaration in the form\
        \ (opaque <kind:symbol> <name:symbol> <annotations>...)"

rememberDeclaration ::
  (MonadFail m, MonadState Env m) => Declaration 'Parsed -> m ()
rememberDeclaration decl = do
  env@Env{userTypes} <- get
  if name `Map.member` userTypes
    then fail $ "duplicate declaration of type " ++ Text.unpack name
    else put env{userTypes = Map.insert name decl userTypes}
  where
    name = declarationName decl

declarationName :: Declaration stage -> TypeName
declarationName = \case
  DAlias Alias{name} -> name
  DEnum Enum{name} -> name
  DOpaqueAtoms Opaque{name} -> name
  DOpaqueObject Opaque{name} -> name
  DStructLww Struct{name} -> name
  DStructSet Struct{name} -> name

instance FromEDN (StructLww Parsed) where
  parseEDN =
    parseStruct
      StructConfig
        { keyword            = "struct_lww"
        , parseMergeStrategy = \vs -> pure (LWW, vs)
        , errorMessage       =
            "Expected field declaration in the form\
            \ (<name:symbol> <type:type_expression>)"
        }

instance FromEDN (StructSet Parsed) where
  parseEDN =
    parseStruct
      StructConfig
        { keyword            = "struct_set"
        , parseMergeStrategy = \case
            mergeStrategyVal : rest -> do
              mergeStrategy <- parseEDN mergeStrategyVal
              pure (mergeStrategy, rest)
            [] -> fail errorMessage
        , errorMessage
        }
    where
      errorMessage =
        "Expected field declaration in the form\
        \ (<name:symbol> <merge_strategy:symbol> <type:type_expression>)"

data StructConfig = StructConfig
  { keyword            :: String
  , parseMergeStrategy :: [TaggedValue] -> Parser (MergeStrategy, [TaggedValue])
  , errorMessage       :: String
  }

parseStruct :: StructConfig -> TaggedValue -> Parser (Struct encoding Parsed)
parseStruct config@StructConfig{keyword} =
  withNoTag . withList $ \case
    nameSym : body -> do
      let (annotationVals, fieldVals) = span isTagged body
      name <- parseSymbol' nameSym
      fields <- parseFields config fieldVals
      annotations <- parseList annotationVals
      pure Struct{..}
    [] ->
      fail $
        "Expected declaration in the form ("
        ++ keyword
        ++ " <name:symbol> <annotations>... <fields>...)"

parseFields ::
  StructConfig -> [Tagged Text Value] -> Parser (Map Text (Field Parsed))
parseFields parseMergeStrategy =
  (`execStateT` mempty) . traverse (parseField parseMergeStrategy)

parseField ::
  StructConfig -> TaggedValue -> StateT (Map Text (Field Parsed)) Parser ()
parseField StructConfig{parseMergeStrategy, errorMessage} tv = do
  NamedField{name, field} <- lift $ parseNamedField tv
  definedAlready <- gets $ Map.member name
  if definedAlready then
    fail $ "Field " <> Text.unpack name <> " is defined already"
  else
    modify' $ Map.insert name field
  where
    parseNamedField = withNoTag . withList $ \case
      nameSym : rest1@(_:_) -> do
        name <- parseEDN nameSym
        (mergeStrategy, rest2) <- parseMergeStrategy rest1
        ronType <- case rest2 of
          [typeVal] -> parseEDN typeVal
          _         -> fail errorMessage
        pure NamedField{name, field = Field{ronType, mergeStrategy, annotations = FieldAnnotations, ext = ()}}
      _ -> fail errorMessage

data NamedField = NamedField
  { name  :: Text
  , field :: Field Parsed
  }

instance FromEDN StructAnnotations where
  parseEDN = withNoTag . withList $ \annTaggedValues -> do
    annValues <- traverse unwrapTag annTaggedValues
    case lookup "haskell" annValues of
      Nothing -> pure defaultStructAnnotations
      Just annValue -> withMap go annValue
    where
      go m = do
        haskellFieldPrefix <- mapGetSymbol "field_prefix" m <|> pure ""
        haskellFieldCaseTransform <- optional $ mapGetSymbol "field_case" m
        pure StructAnnotations{haskellFieldPrefix, haskellFieldCaseTransform}

instance FromEDN CaseTransform where
  parseEDN = withSymbol' $ \case
    "title" -> pure TitleCase
    _ -> fail "unknown case transformation"

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
        [] -> fail "empty type expression"
        f : args -> case f of
          Use typ -> pure $ Apply typ args
          Apply{} ->
            fail "type function must be a name, not expression"
    value -> value `unexpected` "type symbol or expression"

collectDeclarations :: (MonadFail m, MonadState Env m) => Schema Parsed -> m ()
collectDeclarations = traverse_ rememberDeclaration

validateParsed :: MonadFail m => Env -> Schema Parsed -> m ()
validateParsed Env{userTypes} = traverse_ $ \case
  DAlias        Alias{target}  -> validateExpr target
  DEnum         _              -> pure ()
  DOpaqueAtoms  _              -> pure ()
  DOpaqueObject _              -> pure ()
  DStructLww    Struct{fields} -> validateStruct fields
  DStructSet    Struct{fields} -> validateStruct fields
  where
    validateName name =
      unless (name `Map.member` userTypes || name `Map.member` prelude) $
        fail $ "validateParsed: unknown type name " ++ Text.unpack name
    validateExpr = \case
      Use name -> validateName name
      Apply name args -> do
        validateName name
        for_ args validateExpr
    validateStruct = traverse_ $ \Field{ronType} -> validateExpr ronType

validateResolved :: MonadFail m => Schema Resolved -> m ()
validateResolved = traverse_ $ \case
  DAlias _ -> pure ()
  DEnum _ -> pure ()
  DOpaqueAtoms _ -> pure ()
  DOpaqueObject _ -> pure ()
  DStructLww _ -> pure ()
  DStructSet Struct{name, fields} -> validateSetFields name fields
  where
    validateSetFields structName fields =
      void $ Map.traverseWithKey validateField fields
      where
        validateField fieldName field =
          case ronType of
            TAtom        a -> goAtom a
            TEnum        _ -> goAtom TAUuid
            TObject      _ -> goObject
            TOpaqueAtoms _ -> pure ()
          where
            Field{ronType, mergeStrategy} = field
            goAtom a = case (mergeStrategy, a) of
              (DelegateMonoid, _) -> fail' "atoms are not monoid"
              (LWW, _) -> pure ()
              (Set, _) -> pure ()
              (Max, TAInteger) -> pure ()
              (Max, TAFloat) -> pure ()
              (Min, TAInteger) -> pure ()
              (Min, TAFloat) -> pure ()
              (Max, _) ->
                fail' "max strategy requires either integer or float field"
              (Min, _) ->
                fail' "min strategy requires either integer or float field"
            goObject = case mergeStrategy of
              DelegateMonoid -> pure ()
              Set -> pure ()
              LWW -> fail'' "LWW is not safe for objects"
              Max -> fail'' "objects are not ordered"
              Min -> fail'' "objects are not ordered"
            fail' msg =
              fail $ Text.unpack $ structName <> "." <> fieldName <> ": " <> msg
            fail'' msg =
              fail'
              $ msg <> ", valid merge strategies for objects are: monoid, set"

evalSchema :: Env -> Schema 'Resolved
evalSchema env = fst <$> userTypes'
  where
    Env{userTypes} = env
    userTypes' = evalDeclaration <$> userTypes
    evalDeclaration :: Declaration 'Parsed -> (Declaration 'Resolved, RonTypeF)
    evalDeclaration = \case
      DAlias Alias{name, target} ->
        let target' = evalType target
         in (DAlias Alias{name, target = target'}, Type0 target')
      DEnum t -> (DEnum t, Type0 $ TEnum t)
      DOpaqueAtoms t -> (DOpaqueAtoms t, Type0 $ TOpaqueAtoms t)
      DOpaqueObject t -> (DOpaqueObject t, Type0 $ TObject $ TOpaqueObject t)
      DStructLww s ->
        (DStructLww &&& Type0 . TObject . TStructLww) $ evalStruct s
      DStructSet s ->
        (DStructSet &&& Type0 . TObject . TStructSet) $ evalStruct s
    evalStruct :: Struct encoding Parsed -> Struct encoding Resolved
    evalStruct Struct{..} = Struct{fields = evalField <$> fields, ..}
    evalField :: Field Parsed -> Field Resolved
    evalField Field{..} = Field{ronType = evalType ronType, ..}
    getType :: TypeName -> RonTypeF
    getType typ =
      (prelude !? typ)
        <|> (snd <$> userTypes' !? typ)
        ?: error "type is validated but not found"
    evalType = \case
      Use typ -> case getType typ of
        Type0 t0 -> t0
        _ -> error "type arity mismatch"
      Apply typ args -> applyType typ $ evalType <$> args
    applyType name args = case getType name of
      Type0 _ -> error "type arity mismatch"
      Type1 t1 -> case args of
        [a] -> t1 a
        _ ->
          error $
            Text.unpack name ++ " expects 1 argument, got "
              ++ show (length args)
      Type2 t2 -> case args of
        [a, b] -> t2 a b
        _ ->
          error $
            Text.unpack name ++ " expects 2 arguments, got "
              ++ show (length args)

instance FromEDN (Alias 'Parsed) where
  parseEDN = withNoTag . withList $ \case
    [nameSym, targetVal] -> do
      name <- parseSymbol' nameSym
      target <- parseEDN targetVal
      pure Alias{name, target}
    _ ->
      fail
        "Expected declaration in the form\
        \ (alias <name:symbol> <target:type>)"

instance FromEDN FieldAnnotations where
  parseEDN _ = pure defaultFieldAnnotations

unwrapTag :: Tagged Text a -> Parser (Text, a)
unwrapTag = \case
  Tagged prefix tag value ->
    let name = case prefix of
          "" -> tag
          _ -> prefix <> "/" <> tag
     in pure (name, value)
  NoTag _ -> fail "annotation must be a tagged value"

instance FromEDN MergeStrategy where
  parseEDN = withSymbol' $ \case
    "lww" -> pure LWW
    "max" -> pure Max
    "min" -> pure Min
    "set" -> pure Set
    "merge" -> pure DelegateMonoid
    s -> fail $ "unknown merge strategy " <> show s
