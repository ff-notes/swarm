{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RON.Schema.TH.Common (
    let1S,
    liftText,
    mkGuideType,
    mkNameT,
    valD,
    valDP,
) where

import           RON.Prelude

import qualified Data.Text as Text
import           Language.Haskell.TH (conT, normalB, varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftString)

import           RON.Data.ORSet (ORSet (..))
import           RON.Data.RGA (RGA (..))
import           RON.Data.VersionVector (VersionVector)
import           RON.Schema as X

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

valD :: TH.PatQ -> TH.ExpQ -> TH.DecQ
valD pat body = TH.valD pat (normalB body) []

valDP :: TH.Name -> TH.ExpQ -> TH.DecQ
valDP = valD . varP

-- | Guide type is the type which has an instance of 'Replicated'.
-- Different guide types may have same user type, or, from the other side,
-- a user type may be replicated different ways, with different guide types.
mkGuideType :: RonType -> TH.TypeQ
mkGuideType typ = case typ of
    TAtom atom -> case atom of
        TAInteger -> [t| Int64 |]
        TAString  -> [t| Text |]
    TComposite t -> case t of
        TEnum   Enum{name} -> conT $ mkNameT name
        TOption u          -> [t| Maybe $(mkGuideType u) |]
    TObject t -> case t of
        TORSet     a               -> wrap ''ORSet a
        TRga       a               -> wrap ''RGA   a
        TStructLww StructLww{name} -> conT $ mkNameT name
        TVersionVector             -> [t| VersionVector |]
    TOpaque Opaque{name, annotations} -> let
        OpaqueAnnotations{haskellType} = annotations
        in conT $ mkNameT $ fromMaybe name haskellType
  where
    wrap w item = [t| $(conT w) $(mkGuideType item) |]

liftText :: Text -> TH.ExpQ
liftText t = [| Text.pack $(liftString $ Text.unpack t) |]

let1S :: TH.PatQ -> TH.ExpQ -> TH.StmtQ
let1S pat exp = TH.letS [valD pat exp]
