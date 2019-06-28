{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RON.Schema.TH.Struct (mkReplicatedStructLww) where

import           RON.Prelude

import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toTitle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Language.Haskell.TH (Exp (VarE), bindS, conE, conP, conT, doE,
                                      listE, noBindS, recC, recConE, sigD, varE,
                                      varP, varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (Replicated (..), ReplicatedAsObject (..),
                           getObjectStateChunk, objectEncoding)
import           RON.Data.LWW (lwwType)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (ORSet (..))
import           RON.Data.RGA (RGA (..))
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock)
import           RON.Schema as X
import           RON.Schema.TH.Common (liftText, mkGuideType, mkNameT,
                                       mkViewType, valDP)
import           RON.Types (Object (Object), StateFrame, UUID)
import           RON.Util (Instance (..))
import qualified RON.UUID as UUID

data Field' = Field'
    { field'Name     :: Text
    , field'RonName  :: UUID
    , field'Type     :: RonType
    , field'Var      :: TH.Name
    }

mkReplicatedStructLww :: HasCallStack => StructLww 'Resolved -> TH.DecsQ
mkReplicatedStructLww struct = do
    fields <- for (Map.assocs structFields) $ \(field'Name, Field{fieldType}) ->
        case UUID.mkName . BSC.pack $ Text.unpack field'Name of
            Just field'RonName -> do
                field'Var <- TH.newName $ Text.unpack field'Name
                pure Field'{field'Type = fieldType, ..}
            Nothing -> fail $
                "Field name is not representable in RON: " ++ show field'Name
    dataType <- mkDataType
    [instanceReplicated] <- mkInstanceReplicated
    [instanceReplicatedAsObject] <- mkInstanceReplicatedAsObject fields
    accessors <- fold <$> traverse mkAccessors fields
    pure $
        dataType : instanceReplicated : instanceReplicatedAsObject : accessors
  where

    StructLww{structName, structFields, structAnnotations} = struct

    StructAnnotations{saHaskellFieldPrefix, saHaskellFieldCaseTransform} =
        structAnnotations

    name = mkNameT structName

    structT = conT name

    objectT = [t| Object $structT |]

    mkDataType = TH.dataD (TH.cxt []) name [] Nothing
        [recC name
            [ TH.varBangType (mkNameT $ mkHaskellFieldName fieldName) $
                TH.bangType (TH.bang TH.sourceNoUnpack TH.sourceStrict) viewType
            | (fieldName, Field fieldType) <- Map.assocs structFields
            , let viewType = mkViewType fieldType
            ]]
        []

    mkInstanceReplicated = [d|
        instance Replicated $structT where
            encoding = objectEncoding
        |]

    mkInstanceReplicatedAsObject fields = do
        obj   <- TH.newName "obj"
        ops   <- TH.newName "ops"
        let fieldsToUnpack =
                [ bindS var
                    [| LWW.viewField $(liftData field'RonName) $(varE ops) |]
                | Field'{field'Type, field'Var, field'RonName} <- fields
                , let
                    fieldP = varP field'Var
                    var = maybe fieldP (\w -> conP w [fieldP]) $
                        fieldWrapperC field'Type
                ]
        let getObjectImpl = doE
                $   bindS (varP ops) [| getObjectStateChunk $(varE obj) |]
                :   fieldsToUnpack
                ++  [noBindS [| pure $consE |]]
        [d| instance ReplicatedAsObject $structT where
                objectOpType = lwwType
                newObject $consP = Object <$> LWW.newObject $fieldsToPack
                getObject $(varP obj) =
                    errorContext $(liftText errCtx) $getObjectImpl
            |]
      where
        fieldsToPack = listE
            [ [| ($(liftData field'RonName), Instance $var) |]
            | Field'{field'Type, field'Var, field'RonName} <- fields
            , let
                fieldVarE = varE field'Var
                var = case fieldWrapperC field'Type of
                    Nothing  -> fieldVarE
                    Just con -> [| $(conE con) $fieldVarE |]
            ]
        errCtx = "getObject @" <> structName <> ":\n"
        consE = recConE name
            [ pure (fieldName, VarE field'Var)
            | Field'{field'Name, field'Var} <- fields
            , let fieldName = mkNameT $ mkHaskellFieldName field'Name
            ]
        consP = conP name [varP field'Var | Field'{field'Var} <- fields]

    mkHaskellFieldName base = saHaskellFieldPrefix <> base' where
        base' = case saHaskellFieldCaseTransform of
            Nothing        -> base
            Just TitleCase -> case Text.uncons base of
                Nothing            -> base
                Just (b, baseTail) -> Text.cons (toTitle b) baseTail

    mkAccessors field' = do
        a <- varT <$> TH.newName "a"
        m <- varT <$> TH.newName "m"
        let assignF =
                [ sigD assign [t|
                    (ReplicaClock $m, MonadE $m, MonadState StateFrame $m)
                    => $fieldViewType -> $objectT -> $m ()
                    |]
                , valDP assign
                    [| LWW.assignField $(liftData field'RonName) . $guide |]
                ]
            readF =
                [ sigD read [t|
                    (MonadE $m, MonadState StateFrame $m)
                    => $objectT -> $m $fieldViewType
                    |]
                , valDP read
                    [| fmap $unguide . LWW.readField $(liftData field'RonName)
                    |]
                ]
            zoomF =
                [ sigD zoom [t|
                    (MonadE $m, MonadState StateFrame $m)
                    => $objectT
                    -> (Object $(mkGuideType field'Type) -> $m $a)
                    -> $m $a
                    |]
                , valDP zoom [| LWW.zoomField $(liftData field'RonName) |]
                ]
        sequenceA $ assignF ++ readF ++ zoomF
      where
        Field'{field'Name, field'RonName, field'Type} = field'
        fieldViewType = mkViewType field'Type
        assign = mkNameT $ mkHaskellFieldName field'Name <> "_assign"
        read   = mkNameT $ mkHaskellFieldName field'Name <> "_read"
        zoom   = mkNameT $ mkHaskellFieldName field'Name <> "_zoom"
        guidedX = case fieldWrapperC field'Type of
            Just w  -> conP w [x]
            Nothing -> x
          where
            x = varP $ TH.mkName "x"
        unguide = [| \ $guidedX -> x |]
        guide = case fieldWrapperC field'Type of
            Just w  -> conE w
            Nothing -> [| identity |]

-- | Type-directing newtype
fieldWrapperC :: RonType -> Maybe TH.Name
fieldWrapperC typ = case typ of
    TAtom                   _ -> Nothing
    TComposite              _ -> Nothing
    TObject                 t -> case t of
        TORSet              _ -> Just 'ORSet
        TRga                _ -> Just 'RGA
        TStructLww          _ -> Nothing
        TVersionVector        -> Nothing
    TOpaque                 _ -> Nothing
