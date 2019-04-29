{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RON.Schema (
    Alias (..),
    CaseTransform (..),
    Declaration (..),
    Field (..),
    Opaque (..),
    OpaqueAnnotations (..),
    RonType (..),
    Schema,
    Stage (..),
    StructAnnotations (..),
    StructLww (..),
    TAtom (..),
    TComposite (..),
    TEnum (..),
    TObject (..),
    TypeExpr (..),
    TypeName,
    UseType,
    defaultOpaqueAnnotations,
    defaultStructAnnotations,
    opaqueAtoms,
    opaqueAtoms_,
    opaqueObject,
) where

import           RON.Prelude

import qualified Data.Text as Text

data Stage = Parsed | Resolved

type TypeName = Text

data TypeExpr
    = Use TypeName
    | Apply TypeName [TypeExpr]
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
    = TOption RonType
    | TEnum   TEnum
    deriving (Show)

data TEnum = Enum {enumName :: Text, enumItems :: [Text]}
    deriving (Show)

data TObject
    = TORSet     RonType
    | TRga       RonType
    | TStructLww (StructLww 'Resolved)
    | TVersionVector
    deriving (Show)

data StructLww stage = StructLww
    { structName        :: Text
    , structFields      :: Map Text (Field stage)
    , structAnnotations :: StructAnnotations
    }
deriving instance Show (UseType stage) => Show (StructLww stage)

data StructAnnotations = StructAnnotations
    { saHaskellFieldPrefix        :: Text
    , saHaskellFieldCaseTransform :: Maybe CaseTransform
    }
    deriving (Show)

defaultStructAnnotations :: StructAnnotations
defaultStructAnnotations = StructAnnotations
    {saHaskellFieldPrefix = Text.empty, saHaskellFieldCaseTransform = Nothing}

data CaseTransform = TitleCase
    deriving (Show)

newtype Field stage = Field{fieldType :: UseType stage}
deriving instance Show (UseType stage) => Show (Field stage)

type family UseType (stage :: Stage) where
    UseType 'Parsed   = TypeExpr
    UseType 'Resolved = RonType

data Declaration stage
    = DAlias     (Alias stage)
    | DEnum       TEnum
    | DOpaque     Opaque
    | DStructLww (StructLww stage)
deriving instance Show (UseType stage) => Show (Declaration stage)

type family Schema (stage :: Stage) where
    Schema 'Parsed   = [Declaration 'Parsed]
    Schema 'Resolved = Map TypeName (Declaration 'Resolved)

newtype OpaqueAnnotations = OpaqueAnnotations{oaHaskellType :: Maybe Text}
    deriving (Show)

defaultOpaqueAnnotations :: OpaqueAnnotations
defaultOpaqueAnnotations = OpaqueAnnotations{oaHaskellType = Nothing}

data Opaque = Opaque
    { opaqueIsObject    :: Bool
    , opaqueName        :: Text
    , opaqueAnnotations :: OpaqueAnnotations
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
