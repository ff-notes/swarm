{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RON.Schema.TH(
    module X,
    mkReplicated,
    mkReplicated',
) where

import           RON.Prelude

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Language.Haskell.TH (Loc (Loc), conE, conP, conT, lamCaseE,
                                      normalB)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter), quoteDec,
                                            quoteExp, quotePat, quoteType)
import           Language.Haskell.TH.Syntax (dataToPatQ, liftData)

import           RON.Data (Replicated (..), ReplicatedAsPayload (..))
import           RON.Error (throwErrorString)
import           RON.Schema as X
import qualified RON.Schema.EDN as EDN
import           RON.Schema.TH.Common (mkGuideType, mkNameT)
import           RON.Schema.TH.Struct (mkReplicatedStructLww)
import qualified RON.UUID as UUID

-- | QuasiQuoter to generate Haskell types from RON-Schema
mkReplicated :: HasCallStack => QuasiQuoter
mkReplicated = QuasiQuoter{quoteDec, quoteExp = e, quotePat = e, quoteType = e}
  where
    e = error "declaration only"
    quoteDec source = do
        Loc{loc_filename} <- TH.location
        schema <- EDN.readSchema loc_filename $ Text.pack source
        mkReplicated' schema

-- | Generate Haskell types from RON-Schema
mkReplicated' :: HasCallStack => Schema 'Resolved -> TH.DecsQ
mkReplicated' = fmap fold . traverse fromDecl where
    fromDecl decl = case decl of
        DAlias     a -> mkAlias a
        DEnum      e -> mkEnum e
        DOpaque    _ -> pure []
        DStructLww s -> mkReplicatedStructLww s

mkEnum :: TEnum -> TH.DecsQ
mkEnum Enum{enumName, enumItems} = do
    itemsUuids <- for enumItems $ \item -> do
        uuid <- UUID.mkName $ Text.encodeUtf8 item
        pure (mkNameT item, uuid)
    dataType <- mkDataType
    [instanceReplicated] <- mkInstanceReplicated
    [instanceReplicatedAsPayload] <- mkInstanceReplicatedAsPayload itemsUuids
    pure [dataType, instanceReplicated, instanceReplicatedAsPayload]

  where

    typeName = conT $ mkNameT enumName

    mkDataType = TH.dataD (TH.cxt []) (mkNameT enumName) [] Nothing
        [TH.normalC (mkNameT item) [] | item <- enumItems] []

    mkInstanceReplicated = [d|
        instance Replicated $typeName where
            encoding = payloadEncoding
        |]

    mkInstanceReplicatedAsPayload itemsUuids = [d|
        instance ReplicatedAsPayload $typeName where
            toPayload = toPayload . $toUuid
            fromPayload = fromPayload >=> $fromUuid
        |]
      where
        toUuid = lamCaseE
            [match (conP name []) (liftData uuid) | (name, uuid) <- itemsUuids]
        fromUuid = lamCaseE
            $   [ match (liftDataP uuid) [| pure $(conE name) |]
                | (name, uuid) <- itemsUuids
                ]
            ++  [match
                    TH.wildP
                    [| throwErrorString "expected one of enum items" |]]
        liftDataP = dataToPatQ $ const Nothing
        match pat body = TH.match pat (normalB body) []

mkAlias :: Alias Resolved -> TH.DecsQ
mkAlias Alias{name, target} =
    (:[]) <$> TH.tySynD (mkNameT name) [] (mkGuideType target)
