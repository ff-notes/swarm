{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RON.Schema.TH.Common (
    let1S,
    liftText,
    mkGuideType,
    mkNameT,
    newNameT,
    valD,
    valDP,
) where

import           RON.Prelude

import qualified Data.Text as Text
import           Language.Haskell.TH (Q, conT, normalB, varP)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftString)

import           RON.Data.ORSet (ORSet, ORSetMap)
import           RON.Data.RGA (RGA)
import           RON.Data.VersionVector (VersionVector)
import           RON.Schema as X

mkNameT :: Text -> TH.Name
mkNameT = TH.mkName . Text.unpack

newNameT :: Text -> Q TH.Name
newNameT = TH.newName . Text.unpack

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
        TEnum     Enum{name} -> conT $ mkNameT name
        TOption   u          -> [t| Maybe $(mkGuideType u) |]
    TObject t -> case t of
        TORSet     item         -> wrap  ''ORSet    item
        TORSetMap  key value    -> wrap2 ''ORSetMap key value
        TRga       item         -> wrap  ''RGA      item
        TStructLww Struct{name} -> conT $ mkNameT name
        TStructSet Struct{name} -> conT $ mkNameT name
        TVersionVector          -> [t| VersionVector |]
    TOpaque Opaque{name, annotations} -> let
        OpaqueAnnotations{haskellType} = annotations
        in conT $ mkNameT $ fromMaybe name haskellType
  where
    wrap  w a   = [t| $(conT w) $(mkGuideType a) |]
    wrap2 w a b = [t| $(conT w) $(mkGuideType a) $(mkGuideType b) |]

liftText :: Text -> TH.ExpQ
liftText t = [| Text.pack $(liftString $ Text.unpack t) |]

let1S :: TH.PatQ -> TH.ExpQ -> TH.StmtQ
let1S pat exp = TH.letS [valD pat exp]
