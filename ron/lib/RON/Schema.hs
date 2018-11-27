{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RON.Schema
    ( CaseTransform (..)
    , Declaration (..)
    , Field (..)
    , FieldAnnotations (..)
    , OpaqueAnnotations (..)
    , RonType (..)
    , Schema
    , StructAnnotations (..)
    , StructLww (..)
    , TAtom (..)
    , TComposite (..)
    , TObject (..)
    , TOpaque (..)
    , atomInteger
    , atomString
    , boole
    , char
    , def
    , field
    , opaqueAtoms
    , opaqueObject
    , option
    , orSet
    , rgaString
    , structLww
    , versionVector
    ) where

import           RON.Internal.Prelude

import           Data.Default (Default, def)

data TAtom = TAInteger | TAString
    deriving (Show)

data RonType
    = TAtom      TAtom
    | TComposite TComposite
    | TObject    TObject
    | TOpaque    TOpaque
    deriving (Show)

newtype TComposite
    = TOption RonType
    deriving (Show)

data TObject
    = TORSet     RonType
    | TRga       RonType
    | TStructLww StructLww
    | TVersionVector
    deriving (Show)

data StructLww = StructLww
    { structName        :: Text
    , structFields      :: Map Text Field
    , structAnnotations :: StructAnnotations
    }
    deriving (Show)

data StructAnnotations = StructAnnotations
    { saHaskellFieldPrefix        :: Text
    , saHaskellFieldCaseTransform :: Maybe CaseTransform
    }
    deriving (Show)

data CaseTransform = TitleCase
    deriving (Show)

instance Default StructAnnotations where def = StructAnnotations "" Nothing

data Field = Field{fieldType :: RonType, fieldAnnotations :: FieldAnnotations}
    deriving (Show)

field :: RonType -> Field
field fieldType = Field{fieldType, fieldAnnotations = def}

data FieldAnnotations = FieldAnnotations
    deriving (Show)

instance Default FieldAnnotations where
    def = FieldAnnotations

newtype Declaration = DStructLww StructLww

type Schema = [Declaration]

newtype OpaqueAnnotations = OpaqueAnnotations{oaHaskellType :: Maybe Text}
    deriving (Default, Show)

char :: RonType
char = opaqueAtoms def{oaHaskellType = Just "Char"}

rgaString :: RonType
rgaString = TObject $ TRga char

data TOpaque =
    Opaque{opaqueIsObject :: Bool, opaqueAnnotations :: OpaqueAnnotations}
    deriving (Show)

opaqueObject :: OpaqueAnnotations -> RonType
opaqueObject = TOpaque . Opaque True

opaqueAtoms :: OpaqueAnnotations -> RonType
opaqueAtoms = TOpaque . Opaque False

option :: RonType -> RonType
option = TComposite . TOption

structLww :: StructLww -> RonType
structLww = TObject . TStructLww

atomString :: RonType
atomString = TAtom TAString

atomInteger :: RonType
atomInteger = TAtom TAInteger

orSet :: RonType -> RonType
orSet = TObject . TORSet

versionVector :: RonType
versionVector = TObject TVersionVector

boole :: RonType
boole = opaqueAtoms def{oaHaskellType = Just "Bool"}
