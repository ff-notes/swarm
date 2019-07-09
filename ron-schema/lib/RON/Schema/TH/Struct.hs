{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module RON.Schema.TH.Struct (mkReplicatedStructLww) where

import           RON.Prelude

import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toTitle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Language.Haskell.TH (Exp (VarE), bindS, conP, conT, doE,
                                      listE, noBindS, recC, recConE, sigD, varE,
                                      varP, varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (MonadObjectState, ObjectStateT, Replicated (..),
                           ReplicatedAsObject (..), getObjectStateChunk,
                           objectEncoding)
import           RON.Data.LWW (lwwType)
import qualified RON.Data.LWW as LWW
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock)
import           RON.Schema as X
import           RON.Schema.TH.Common (liftText, mkGuideType, mkNameT, valDP)
import           RON.Types (Object (Object), UUID)
import           RON.Util (Instance (..))
import qualified RON.UUID as UUID

data Field' = Field'
    { field'Name     :: Text
    , field'RonName  :: UUID
    , field'Type     :: RonType
    , field'Var      :: TH.Name
    }

mkReplicatedStructLww :: StructLww 'Resolved -> TH.DecsQ
mkReplicatedStructLww StructLww{name, fields, annotations} = do
    fields' <- for (Map.assocs fields) $ \(field'Name, Field{type_}) ->
        case UUID.mkName . BSC.pack $ Text.unpack field'Name of
            Just field'RonName -> do
                field'Var <- TH.newName $ Text.unpack field'Name
                pure Field'{field'Type = type_, ..}
            Nothing -> fail $
                "Field name is not representable in RON: " ++ show field'Name
    dataType <- mkDataType structName fields annotations
    [instanceReplicated] <- mkInstanceReplicated structType
    [instanceReplicatedAsObject] <-
        mkInstanceReplicatedAsObject name fields' annotations
    accessors <- fold <$> traverse (mkAccessors structType annotations) fields'
    pure $
        dataType : instanceReplicated : instanceReplicatedAsObject : accessors
  where
    structName = mkNameT name
    structType = conT structName

mkDataType
    :: TH.Name -> Map Text (Field Resolved) -> StructAnnotations -> TH.DecQ
mkDataType name fields annotations = TH.dataD (TH.cxt []) name [] Nothing
    [recC name
        [ TH.varBangType (mkNameT $ mkHaskellFieldName annotations fieldName) $
            TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) guideType
        | (fieldName, Field ronType) <- Map.assocs fields
        , let guideType = mkGuideType ronType
        ]]
    []

mkInstanceReplicated :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicated structType = [d|
    instance Replicated $structType where
        encoding = objectEncoding
    |]

mkInstanceReplicatedAsObject
    :: Text -> [Field'] -> StructAnnotations -> TH.DecsQ
mkInstanceReplicatedAsObject name fields annotations = do
    ops <- TH.newName "ops"
    let fieldsToUnpack =
            [ bindS fieldVarP
                [| LWW.viewField $(liftData field'RonName) $(varE ops) |]
            | Field'{field'Var, field'RonName} <- fields
            , let
                fieldVarP = varP field'Var
                -- var = maybe fieldP (\w -> conP w [fieldP]) $
                --     fieldWrapperC field'Type
            ]
    let getObjectImpl = doE
            $   bindS (varP ops) [| getObjectStateChunk |]
            :   fieldsToUnpack
            ++  [noBindS [| pure $consE |]]
    [d| instance ReplicatedAsObject $structType where
            objectOpType = lwwType
            newObject $consP = Object <$> LWW.newObject $fieldsToPack
            getObject =
                errorContext $(liftText errCtx) $getObjectImpl
        |]
  where
    name' = mkNameT name
    structType = conT name'
    fieldsToPack = listE
        [ [| ($(liftData field'RonName), Instance $fieldVarE) |]
        | Field'{field'Var, field'RonName} <- fields
        , let
            fieldVarE = varE field'Var
            -- var = case fieldWrapperC field'Type of
            --     Nothing  -> fieldVarE
            --     Just con -> [| $(conE con) $fieldVarE |]
        ]
    errCtx = "getObject @" <> name <> ":\n"
    consE = recConE name'
        [ pure (fieldName, VarE field'Var)
        | Field'{field'Name, field'Var} <- fields
        , let fieldName = mkNameT $ mkHaskellFieldName annotations field'Name
        ]
    consP = conP name' [varP field'Var | Field'{field'Var} <- fields]

mkHaskellFieldName :: StructAnnotations -> Text -> Text
mkHaskellFieldName annotations base = prefix <> base' where
    StructAnnotations
            { haskellFieldPrefix        = prefix
            , haskellFieldCaseTransform = caseTransform
            }
        = annotations
    base' = case caseTransform of
        Nothing        -> base
        Just TitleCase -> case Text.uncons base of
            Nothing            -> base
            Just (b, baseTail) -> Text.cons (toTitle b) baseTail

mkAccessors :: TH.TypeQ -> StructAnnotations -> Field' -> TH.DecsQ
mkAccessors structType annotations field' = do
    a <- varT <$> TH.newName "a"
    m <- varT <$> TH.newName "m"
    let assignF =
            [ sigD assign [t|
                (ReplicaClock $m, MonadE $m, MonadObjectState $structType $m)
                => $fieldGuideType -> $m ()
                |]
            , valDP assign [| LWW.assignField $(liftData field'RonName) |]
            ]
        readF =
            [ sigD read [t|
                (MonadE $m, MonadObjectState $structType $m)
                => $m $fieldGuideType
                |]
            , valDP read [| LWW.readField $(liftData field'RonName) |]
            ]
        zoomF =
            [ sigD zoom [t|
                MonadE $m
                => ObjectStateT $(mkGuideType field'Type) $m $a
                -> ObjectStateT $structType                  $m $a
                |]
            , valDP zoom [| LWW.zoomField $(liftData field'RonName) |]
            ]
    sequenceA $ assignF ++ readF ++ zoomF
  where
    Field'{field'Name, field'RonName, field'Type} = field'
    fieldGuideType = mkGuideType field'Type
    assign = mkNameT $ mkHaskellFieldName annotations field'Name <> "_assign"
    read   = mkNameT $ mkHaskellFieldName annotations field'Name <> "_read"
    zoom   = mkNameT $ mkHaskellFieldName annotations field'Name <> "_zoom"
    -- guidedX = case fieldWrapperC field'Type of
    --     Just w  -> conP w [x]
    --     Nothing -> x
    --     where
    --     x = varP $ TH.mkName "x"
    -- unguide = [| \ $guidedX -> x |]
    -- guide = case fieldWrapperC field'Type of
    --     Just w  -> conE w
    --     Nothing -> [| id |]

-- | Type-directing newtype
-- fieldWrapperC :: RonType -> Maybe TH.Name
-- fieldWrapperC typ = case typ of
--     TAtom                   _ -> Nothing
--     TComposite              _ -> Nothing
--     TObject                 t -> case t of
--         TORSet              _ -> Just 'ORSet
--         TRga                _ -> Just 'RGA
--         TStructLww          _ -> Nothing
--         TVersionVector        -> Nothing
--     TOpaque                 _ -> Nothing
