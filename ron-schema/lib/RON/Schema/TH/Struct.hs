{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
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

data instance Field Equipped = FieldEquipped
    { ronType     :: RonType
    -- TODO use extension type FieldX from "Trees that grow"
    , haskellName :: Text
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
    -- [instanceSemigroup]    <- mkInstanceSemigroup       name' (Map.keys fields)
    -- [instanceMonoid]       <- mkInstanceMonoid          name' (Map.keys fields)
    [instanceReplicated]   <- mkInstanceReplicated      type'
    [instanceReplicatedBS] <- mkInstanceReplicatedBS    type'
    [instanceReplicatedAO] <- mkInstanceReplicatedAOSet struct
    accessors <- fold <$> traverse (mkAccessorsSet name' annotations) fields
    pure
        $ dataType
        -- : instanceSemigroup  : instanceMonoid
        : instanceReplicated : instanceReplicatedBS : instanceReplicatedAO
        : accessors
  where
    struct@Struct{name, fields, annotations} = equipStruct structResolved
    name' = mkNameT name
    type' = conT    name'

equipField :: Text -> Field Resolved -> Field Equipped
equipField haskellName FieldResolved{ronType} =
    case UUID.mkName $ Text.encodeUtf8 haskellName of
        Just ronName -> FieldEquipped{haskellName, ronName, ronType}
        Nothing -> error $
            "Field name is not representable in RON: " ++ show haskellName

mkDataTypeLww :: StructLww Equipped -> TH.DecQ
mkDataTypeLww Struct{name, fields, annotations} =
    TH.dataD
        (TH.cxt [])
        name'
        []
        Nothing
        [recC name'
            [ TH.varBangType (mkNameT $ mkHaskellFieldName annotations fieldName) $
                TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) $
                mkGuideType ronType
            | (fieldName, FieldEquipped{ronType}) <- Map.assocs fields
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
            [ TH.varBangType (mkNameT $ mkHaskellFieldName annotations fieldName) $
                TH.bangType
                    (TH.bang TH.sourceNoUnpack TH.sourceStrict)
                    [t| Maybe $(mkGuideType ronType) |]
            | (fieldName, FieldEquipped{ronType}) <- Map.assocs fields
            ]]
        []
  where
    name' = mkNameT name

-- mkInstanceSemigroup :: TH.Name -> [Text] -> TH.Q [TH.Dec]
-- mkInstanceSemigroup typeName fieldNames = do
--     (p1Names, p1) <- mkPat
--     (p2Names, p2) <- mkPat
--     let e   = recConE typeName
--             $ zipWith fieldExp (map mkNameT fieldNames)
--             $ zipWith append p1Names p2Names
--     [d| instance Semigroup $(conT typeName) where
--             $p1 <> $p2 = $e
--         |]
--   where
--     mkPat = do
--         names <- traverse (newName . Text.unpack) fieldNames
--         pure
--             ( names
--             , TH.recP typeName $
--                 zipWith TH.fieldPat (map mkNameT fieldNames) (map varP names)
--             )
--     append x y = [| $(varE x) <> $(varE y) |]

-- mkInstanceMonoid :: TH.Name -> [Text] -> TH.Q [TH.Dec]
-- mkInstanceMonoid typeName fieldNames =
--     [d| instance Monoid $(conT typeName) where
--             mempty = $e
--         |]
--   where
--     e = recConE typeName
--             [ fieldExp (mkNameT fieldName) [| [] |]
--             | fieldName <- fieldNames
--             ]

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
    vars <- traverse (newNameT . haskellName) fields
    let packFields = listE
            [ [| ($ronName', Instance $(varE var)) |]
            | FieldEquipped{ronName} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
        unpackFields =
            [ bindS (varP var) [| LWW.viewField $ronName' $(varE ops) |]
            | FieldEquipped{ronName} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
    let consE = recConE name'
            [ fieldExp fieldName $ varE var
            | FieldEquipped{haskellName} <- toList fields
            , let
                fieldName = mkNameT $ mkHaskellFieldName annotations haskellName
            | var <- toList vars
            ]
        consP = recP name'
            [ fieldPat fieldName $ varP var
            | FieldEquipped{haskellName} <- toList fields
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
    vars <- traverse (newNameT . haskellName) fields
    let packFields = listE
            [ [| ($ronName', fmap Instance $(varE var)) |]
            | FieldEquipped{ronName} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
        unpackFields =
            [ bindS
                (varP var)
                [| errorContext $(liftText haskellName) $
                    $(orSetViewField ronType) $ronName' $(varE ops) |]
            | FieldEquipped{haskellName, ronName, ronType} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
    let consE = recConE name'
            [ fieldExp fieldName $ varE var
            | FieldEquipped{haskellName} <- toList fields
            , let
                fieldName = mkNameT $ mkHaskellFieldName annotations haskellName
            | var <- toList vars
            ]
        consP = recP name'
            [ fieldPat fieldName $ varP var
            | FieldEquipped{haskellName} <- toList fields
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
    FieldEquipped{haskellName, ronName, ronType} = field
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
                    $(orSetViewField ronType) $ronName' chunk |]
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
    FieldEquipped{haskellName, ronName, ronType} = field
    ronName' = liftData ronName
    type'    = conT name'
    fieldGuideType = mkGuideType ronType
    assign = mkNameT $ mkHaskellFieldName annotations haskellName <> "_assign"
    read   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_read"
    zoom   = mkNameT $ mkHaskellFieldName annotations haskellName <> "_zoom"

orSetViewField :: RonType -> TH.ExpQ
orSetViewField ronType = case mergeStrategy ronType of
    ReplicatedBoundedSemilattice -> varE 'ORSet.viewFieldMerge
    LWW                          -> varE 'ORSet.viewFieldLWW
