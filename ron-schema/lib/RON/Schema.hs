{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RON.Schema
  ( Alias (..),
    CaseTransform (..),
    Declaration (..),
    Field (..),
    XField,
    FieldAnnotations (..),
    defaultFieldAnnotations,
    MergeStrategy (..),
    Opaque (..),
    opaqueAtoms,
    opaqueAtoms_,
    opaqueObject,
    OpaqueAnnotations (..),
    defaultOpaqueAnnotations,
    RonType (..),
    Schema,
    Stage (..),
    Struct (..),
    StructAnnotations (..),
    defaultStructAnnotations,
    StructEncoding (..),
    StructLww,
    StructSet,
    TAtom (..),
    TEnum (..),
    TObject (..),
    TypeExpr (..),
    TypeName,
    UseType,
  )
where

import qualified Data.Text as Text
import RON.Prelude

data Stage = Parsed | Resolved | Equipped

type TypeName = Text

data TypeExpr = Use TypeName | Apply TypeName [TypeExpr]
  deriving (Show)

data TAtom = TAFloat | TAInteger | TAString | TAUuid | TObjectRef RonType
  deriving (Show)

data RonType
  = TAtom TAtom
  | TEnum TEnum
  | TObject TObject
  | TOpaqueAtoms Opaque
  deriving (Show)

data TEnum = Enum {name :: Text, items :: [Text]}
  deriving (Show)

data TObject
  = TOpaqueObject Opaque
  | TORSet RonType
  | TORSetMap RonType RonType
  | TRga RonType
  | TStructLww (StructLww Resolved)
  | TStructSet (StructSet Resolved)
  | TVersionVector
  deriving (Show)

data StructEncoding = SELww | SESet

data Struct (encoding :: StructEncoding) stage = Struct
  { name :: Text
  , fields :: Map Text (Field stage)
  , annotations :: StructAnnotations
  }

deriving instance
  (Show (UseType stage), Show (XField stage)) => Show (Struct encoding stage)

type StructLww = Struct SELww

type StructSet = Struct SESet

data StructAnnotations
  = StructAnnotations
      { haskellFieldPrefix :: Text,
        haskellFieldCaseTransform :: Maybe CaseTransform
      }
  deriving (Show)

defaultStructAnnotations :: StructAnnotations
defaultStructAnnotations = StructAnnotations
  { haskellFieldPrefix = Text.empty,
    haskellFieldCaseTransform = Nothing
  }

data CaseTransform = TitleCase
  deriving (Show)

data Field (stage :: Stage) = Field
  { mergeStrategy :: MergeStrategy
    -- ^ Used only for 'StructSet'; for 'StructLww' must be 'LWW'
  , ronType     :: UseType stage
  , annotations :: FieldAnnotations
  , ext         :: XField stage
  }

deriving instance
  (Show (UseType stage), Show (XField stage)) => Show (Field stage)

data FieldAnnotations = FieldAnnotations deriving (Show)

defaultFieldAnnotations :: FieldAnnotations
defaultFieldAnnotations = FieldAnnotations

type family XField (stage :: Stage)

type instance XField Parsed = ()

type instance XField Resolved = ()

type family UseType (stage :: Stage) where
  UseType Parsed = TypeExpr
  UseType Resolved = RonType
  UseType Equipped = RonType

data Declaration stage
  = DAlias (Alias stage)
  | DEnum TEnum
  | DOpaqueAtoms Opaque
  | DOpaqueObject Opaque
  | DStructLww (StructLww stage)
  | DStructSet (StructSet stage)

deriving instance
  (Show (StructLww stage), Show (StructSet stage), Show (UseType stage)) =>
  Show (Declaration stage)

type family Schema (stage :: Stage) where
  Schema 'Parsed = [Declaration 'Parsed]
  Schema 'Resolved = Map TypeName (Declaration 'Resolved)

newtype OpaqueAnnotations = OpaqueAnnotations {haskellType :: Maybe Text}
  deriving (Show)

defaultOpaqueAnnotations :: OpaqueAnnotations
defaultOpaqueAnnotations = OpaqueAnnotations {haskellType = Nothing}

data Opaque = Opaque {name :: Text, annotations :: OpaqueAnnotations}
  deriving (Show)

opaqueObject :: Text -> OpaqueAnnotations -> RonType
opaqueObject tyname = TObject . TOpaqueObject . Opaque tyname

opaqueAtoms :: Text -> OpaqueAnnotations -> RonType
opaqueAtoms tyname = TOpaqueAtoms . Opaque tyname

opaqueAtoms_ :: Text -> RonType
opaqueAtoms_ tyname = TOpaqueAtoms $ Opaque tyname defaultOpaqueAnnotations

data Alias stage = Alias {name :: Text, target :: UseType stage}

deriving instance Show (UseType stage) => Show (Alias stage)

data MergeStrategy = LWW | Max | Min | Set | DelegateMonoid
  deriving (Eq, Show)
