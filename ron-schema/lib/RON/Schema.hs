{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RON.Schema (
    Alias (..),
    CaseTransform (..),
    Declaration (..),
    Field (..),
    MergeStrategy (..),
    Opaque (..),
    OpaqueAnnotations (..),
    RonType (..),
    Schema,
    Stage (..),
    Struct (..),
    StructAnnotations (..),
    StructEncoding (..),
    StructLww,
    StructSet,
    TAtom (..),
    TComposite (..),
    TEnum (..),
    TObject (..),
    TypeExpr (..),
    TypeName,
    UseType,
    defaultOpaqueAnnotations,
    defaultStructAnnotations,
    mergeStrategy,
    opaqueAtoms,
    opaqueAtoms_,
    opaqueObject,
) where

import           RON.Prelude

import qualified Data.Text as Text

data Stage = Parsed | Resolved | Equipped

type TypeName = Text

data TypeExpr = Use TypeName | Apply TypeName [TypeExpr]
    deriving (Show)

data TAtom = TAInteger | TAString
    deriving (Show)

data RonType
    = TAtom      TAtom
    | TComposite TComposite
    | TObject    TObject
    | TOpaque    Opaque
    deriving (Show)

data TComposite
    = TEnum   TEnum
    | TOption RonType
    deriving (Show)

data TEnum = Enum {name :: Text, items :: [Text]}
    deriving (Show)

data TObject
    = TORSet     RonType
    | TORSetMap  RonType RonType
    | TRga       RonType
    | TStructLww (StructLww Resolved)
    | TStructSet (StructSet Resolved)
    | TVersionVector
    deriving (Show)

data StructEncoding = SELww | SESet

data Struct (encoding :: StructEncoding) stage = Struct
    { name        :: Text
    , fields      :: Map Text (Field stage)
    , annotations :: StructAnnotations
    }
deriving instance Show (Field stage) => Show (Struct encoding stage)

type StructLww = Struct SELww

type StructSet = Struct SESet

data StructAnnotations = StructAnnotations
    { haskellFieldPrefix        :: Text
    , haskellFieldCaseTransform :: Maybe CaseTransform
    }
    deriving (Show)

defaultStructAnnotations :: StructAnnotations
defaultStructAnnotations = StructAnnotations
    {haskellFieldPrefix = Text.empty, haskellFieldCaseTransform = Nothing}

data CaseTransform = TitleCase
    deriving (Show)

data family Field (stage :: Stage)

newtype instance Field Parsed = FieldParsed{ronType :: UseType Parsed}
    deriving (Show)

newtype instance Field Resolved = FieldResolved{ronType :: UseType Resolved}
    deriving (Show)

type family UseType (stage :: Stage) where
    UseType 'Parsed   = TypeExpr
    UseType 'Resolved = RonType

data Declaration stage
    = DAlias     (Alias stage)
    | DEnum       TEnum
    | DOpaque     Opaque
    | DStructLww (StructLww stage)
    | DStructSet (StructSet stage)
deriving instance
    (Show (Field stage), Show (UseType stage)) => Show (Declaration stage)

type family Schema (stage :: Stage) where
    Schema 'Parsed   = [Declaration 'Parsed]
    Schema 'Resolved = Map TypeName (Declaration 'Resolved)

newtype OpaqueAnnotations = OpaqueAnnotations{haskellType :: Maybe Text}
    deriving (Show)

defaultOpaqueAnnotations :: OpaqueAnnotations
defaultOpaqueAnnotations = OpaqueAnnotations{haskellType = Nothing}

data Opaque = Opaque
    { isObject    :: Bool
    , name        :: Text
    , annotations :: OpaqueAnnotations
    }
    deriving (Show)

opaqueObject :: Text -> OpaqueAnnotations -> RonType
opaqueObject tyname = TOpaque . Opaque True tyname

opaqueAtoms :: Text -> OpaqueAnnotations -> RonType
opaqueAtoms tyname = TOpaque . Opaque False tyname

opaqueAtoms_ :: Text -> RonType
opaqueAtoms_ tyname = TOpaque $ Opaque False tyname defaultOpaqueAnnotations

data Alias stage = Alias{name :: Text, target :: UseType stage}
deriving instance Show (UseType stage) => Show (Alias stage)

data MergeStrategy
    = ReplicatedBoundedSemilattice
    | LWW

mergeStrategy :: RonType -> MergeStrategy
mergeStrategy = \case
    TAtom       _                -> LWW
    TComposite  t                -> case t of
        TEnum   _                -> LWW
        TOption _                -> ReplicatedBoundedSemilattice
    TObject     _                -> ReplicatedBoundedSemilattice
    TOpaque     Opaque{isObject}
        | isObject               -> ReplicatedBoundedSemilattice
        | otherwise              -> LWW
