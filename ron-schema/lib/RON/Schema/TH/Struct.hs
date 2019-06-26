{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Reduce duplication" -}

module RON.Schema.TH.Struct (
    mkReplicatedStructLww,
    mkReplicatedStructSet,
) where

import           RON.Prelude

import           Data.Char (toTitle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Language.Haskell.TH (bindS, conT, doE, fieldExp, fieldPat,
                                      listE, newName, noBindS, recC, recConE,
                                      recP, sigD, varE, varP, varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (MonadObjectState, ObjectStateT, Rep, Replicated,
                           ReplicatedAsObject, ReplicatedBoundedSemilattice,
                           encoding, getObject, getObjectStateChunk, newObject,
                           objectEncoding, objectRconcat, rconcat)
import           RON.Data.LWW (LwwRep)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (ORSetRep)
import qualified RON.Data.ORSet as ORSet
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock)
import           RON.Schema as X
import           RON.Schema.TH.Common (liftText, mkGuideType, mkNameT, newNameT,
                                       valDP)
import           RON.Types (Object (Object), UUID)
import           RON.Util (Instance (Instance))
import qualified RON.UUID as UUID

type instance XField Equipped = XFieldEquipped

data XFieldEquipped = XFieldEquipped
    { haskellName :: Text
    , ronName     :: UUID
    }

equipStruct :: Struct e Resolved -> Struct e Equipped
equipStruct Struct{name, fields, annotations} =
    Struct{name, fields = Map.mapWithKey equipField fields, annotations}

mkReplicatedStructLww :: StructLww Resolved -> TH.DecsQ
mkReplicatedStructLww structResolved = do
    dataType               <- mkDataTypeLww             struct
    [instanceReplicated]   <- mkInstanceReplicated      type'
    [instanceReplicatedBS] <- mkInstanceReplicatedBS    type'
    [instanceReplicatedAO] <- mkInstanceReplicatedAOLww struct
    accessors <- fold <$> traverse (mkAccessorsLww name' annotations) fields
    pure
        $ dataType
        : instanceReplicated : instanceReplicatedBS : instanceReplicatedAO
        : accessors
  where
    struct@Struct{name, fields, annotations} = equipStruct structResolved
    name' = mkNameT name
    type' = conT    name'

mkReplicatedStructSet :: StructSet Resolved -> TH.DecsQ
mkReplicatedStructSet structResolved = do
    dataType               <- mkDataTypeSet             struct
    [instanceReplicated]   <- mkInstanceReplicated      type'
    [instanceReplicatedBS] <- mkInstanceReplicatedBS    type'
    [instanceReplicatedAO] <- mkInstanceReplicatedAOSet struct
    accessors <- fold <$> traverse (mkAccessorsSet name' annotations) fields
    pure
        $ dataType
        : instanceReplicated : instanceReplicatedBS : instanceReplicatedAO
        : accessors
  where
    struct@Struct{name, fields, annotations} = equipStruct structResolved
    name' = mkNameT name
    type' = conT    name'

equipField :: Text -> Field Resolved -> Field Equipped
equipField haskellName Field{..} =
    case UUID.mkName $ Text.encodeUtf8 haskellName of
        Just ronName -> Field{ext = XFieldEquipped{haskellName, ronName}, ..}
        Nothing -> error $
            "Field name is not representable in RON: " ++ show haskellName

varBangType' :: Text -> TH.TypeQ -> TH.VarBangTypeQ
varBangType' name
    = TH.varBangType (mkNameT name)
    . TH.bangType (TH.bang TH.noSourceUnpackedness TH.sourceStrict)

mkDataTypeLww :: StructLww Equipped -> TH.DecQ
mkDataTypeLww Struct{name, fields, annotations} =
    TH.dataD
        (TH.cxt [])
        name'
        []
        Nothing
        [recC name'
            [ varBangType' (mkHaskellFieldName annotations fieldName) $
                mkGuideType ronType
            | (fieldName, Field{ronType}) <- Map.assocs fields
            ]]
        []
  where
    name' = mkNameT name

mkDataTypeSet :: StructSet Equipped -> TH.DecQ
mkDataTypeSet Struct{name, fields, annotations} =
    TH.dataD
        (TH.cxt [])
        name'
        []
        Nothing
        [recC name'
            [ varBangType'
                (mkHaskellFieldName annotations fieldName)
                [t| Maybe $(mkGuideType ronType) |]
            | (fieldName, Field{ronType}) <- Map.assocs fields
            ]]
        []
  where
    name' = mkNameT name

mkInstanceReplicated :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicated type' = [d|
    instance Replicated $type' where
        encoding = objectEncoding
    |]

mkInstanceReplicatedBS :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicatedBS type' = [d|
    instance ReplicatedBoundedSemilattice $type' where
        rconcat = objectRconcat
    |]

mkInstanceReplicatedAOLww :: StructLww Equipped -> TH.DecsQ
mkInstanceReplicatedAOLww Struct{name, fields, annotations} = do
    ops  <- newName "ops"
    vars <- traverse (newNameT . haskellName . ext) fields
    let packFields = listE
            [ [| ($ronName', Instance $(varE var)) |]
            | Field{ext = XFieldEquipped{ronName}} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
        unpackFields =
            [ bindS (varP var) [| LWW.viewField $ronName' $(varE ops) |]
            | Field{ext = XFieldEquipped{ronName}} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
    let consE = recConE name'
            [ fieldExp fieldName $ varE var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            , let
                fieldName = mkNameT $ mkHaskellFieldName annotations haskellName
            | var <- toList vars
            ]
        consP = recP name'
            [ fieldPat fieldName $ varP var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            , let
                fieldName = mkNameT $ mkHaskellFieldName annotations haskellName
            | var <- toList vars
            ]
    let getObjectImpl = doE
            $   bindS (varP ops) [| getObjectStateChunk |]
            :   unpackFields
            ++  [noBindS [| pure $consE |]]
    [d| instance ReplicatedAsObject $type' where
            type Rep $type' = LwwRep
            newObject $consP = Object <$> LWW.newObject $packFields
            getObject = errorContext $(liftText errCtx) $getObjectImpl
        |]
  where
    name' = mkNameT name
    type' = conT    name'
    errCtx = "getObject @" <> name

mkInstanceReplicatedAOSet :: StructSet Equipped -> TH.DecsQ
mkInstanceReplicatedAOSet Struct{name, fields, annotations} = do
    ops  <- newName "ops"
    vars <- traverse (newNameT . haskellName . ext) fields
    let packFields = listE
            [ [| ($ronName', fmap Instance $(varE var)) |]
            | Field{ext = XFieldEquipped{ronName}} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
        unpackFields =
            [ bindS
                (varP var)
                [| errorContext $(liftText haskellName) $
                    $(orSetViewField mergeStrategy) $ronName' $(varE ops) |]
            | Field{annotations = FieldAnnotations{mergeStrategy}, ext} <-
                toList fields
            , let
                XFieldEquipped{haskellName, ronName} = ext
                ronName' = liftData ronName
            | var <- toList vars
            ]
    let consE = recConE name'
            [ fieldExp fieldName $ varE var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            , let
                fieldName = mkNameT $ mkHaskellFieldName annotations haskellName
            | var <- toList vars
            ]
        consP = recP name'
            [ fieldPat fieldName $ varP var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            , let
                fieldName = mkNameT $ mkHaskellFieldName annotations haskellName
            | var <- toList vars
            ]
    let getObjectImpl = doE
            $   bindS (varP ops) [| getObjectStateChunk |]
            :   unpackFields
            ++  [noBindS [| pure $consE |]]
    [d| instance ReplicatedAsObject $type' where
            type Rep $type' = ORSetRep
            newObject $consP = Object <$> ORSet.newStruct $packFields
            getObject = errorContext $(liftText errCtx) $getObjectImpl
        |]
  where
    name' = mkNameT name
    type' = conT name'
    errCtx = "getObject @" <> name

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

mkAccessorsLww :: TH.Name -> StructAnnotations -> Field Equipped -> TH.DecsQ
mkAccessorsLww name' annotations field = do
    a <- varT <$> newName "a"
    m <- varT <$> newName "m"
    let assignF =
            [ sigD assign [t|
                (ReplicaClock $m, MonadE $m, MonadObjectState $type' $m)
                => $fieldGuideType -> $m () |]
            , valDP assign [| LWW.assignField $ronName' |]
            ]
        readF =
            [ sigD read [t|
                (MonadE $m, MonadObjectState $type' $m) => $m $fieldGuideType |]
            , valDP read [| LWW.readField $ronName' |]
            ]
        zoomF =
            [ sigD zoom [t|
                MonadE $m
                => ObjectStateT $(mkGuideType ronType) $m $a
                -> ObjectStateT $type'                 $m $a |]
            , valDP zoom [| LWW.zoomField $ronName' |]
            ]
    sequenceA $ assignF ++ readF ++ zoomF
  where
    Field{ronType, ext = XFieldEquipped{haskellName, ronName}} = field
    ronName' = liftData ronName
    type'    = conT name'
    fieldGuideType = mkGuideType ronType
    assign = mkNameT $ mkHaskellFieldName annotations haskellName <> "_assign"
    read   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_read"
    zoom   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_zoom"

mkAccessorsSet :: TH.Name -> StructAnnotations -> Field Equipped -> TH.DecsQ
mkAccessorsSet name' annotations field = do
    a <- varT <$> newName "a"
    m <- varT <$> newName "m"
    let assignF =
            [ sigD assign [t|
                (ReplicaClock $m, MonadE $m, MonadObjectState $type' $m)
                => $fieldGuideType -> $m () |]
            , valDP assign [| ORSet.assignField $ronName' |]
            ]
        readF =
            [ sigD read [t|
                (MonadE $m, MonadObjectState $type' $m)
                => $m (Maybe $fieldGuideType) |]
            , valDP read
                [| do
                    chunk <- getObjectStateChunk
                    $(orSetViewField mergeStrategy) $ronName' chunk |]
            ]
        zoomF =
            [ sigD zoom [t|
                MonadE $m
                => ObjectStateT $(mkGuideType ronType) $m $a
                -> ObjectStateT $type'                 $m $a |]
            , valDP zoom [| ORSet.zoomField $ronName' |]
            ]
    sequenceA $ assignF ++ readF ++ zoomF
  where
    Field{ronType, annotations = FieldAnnotations{mergeStrategy}, ext} = field
    XFieldEquipped{haskellName, ronName} = ext
    ronName' = liftData ronName
    type'    = conT name'
    fieldGuideType = mkGuideType ronType
    assign = mkNameT $ mkHaskellFieldName annotations haskellName <> "_assign"
    read   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_read"
    zoom   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_zoom"

orSetViewField :: Maybe MergeStrategy -> TH.ExpQ
orSetViewField = varE . \case
    Nothing  -> 'ORSet.viewField
    Just LWW -> 'ORSet.viewFieldLWW
    Just Max -> 'ORSet.viewFieldMax
    Just Min -> 'ORSet.viewFieldMin
    Just Set -> 'ORSet.viewFieldSet
