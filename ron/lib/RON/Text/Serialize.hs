{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | RON-Text serialization
module RON.Text.Serialize
    ( serializeAtom
    , serializeObject
    , serializeRawOp
    , serializeStateFrame
    , serializeString
    , serializeUuid
    , serializeWireFrame
    , serializeWireFrames
    ) where

import           RON.Prelude

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import           RON.Text.Serialize.UUID (serializeUuid, serializeUuidAtom,
                                          serializeUuidKey)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid),
                            ClosedOp (..), Object (..), Op (..),
                            StateChunk (..), StateFrame,
                            WireChunk (Closed, Query, Value), WireFrame,
                            WireReducedChunk (..))
import           RON.Util (ByteStringL)
import           RON.UUID (UUID, zero)

-- | Serialize a common frame
serializeWireFrame :: WireFrame -> ByteStringL
serializeWireFrame chunks
    = (`BSLC.snoc` '.')
    . fold
    . (`evalState` opZero)
    $ traverse serializeChunk chunks

-- | Serialize a sequence of common frames
serializeWireFrames :: [WireFrame] -> ByteStringL
serializeWireFrames = foldMap serializeWireFrame

-- | Serialize a common chunk
serializeChunk :: WireChunk -> State ClosedOp ByteStringL
serializeChunk = \case
    Closed op   -> (<> " ;\n") <$> serializeClosedOpZip op
    Value chunk -> serializeReducedChunk False chunk
    Query chunk -> serializeReducedChunk True  chunk

-- | Serialize a reduced chunk
serializeReducedChunk :: Bool -> WireReducedChunk -> State ClosedOp ByteStringL
serializeReducedChunk isQuery WireReducedChunk{wrcHeader, wrcBody} =
    BSLC.unlines <$> liftA2 (:) serializeHeader serializeBody
  where
    serializeHeader = do
        h <- serializeClosedOpZip wrcHeader
        pure $ BSLC.unwords [h, if isQuery then "?" else "!"]
    serializeBody = state $ \ClosedOp{op = opBefore, ..} -> let
        (body, opAfter) =
            (`runState` opBefore) $
            for wrcBody $
            fmap ("\t" <>) . serializeReducedOpZip objectId
        in
        ( body
        , ClosedOp{op = opAfter, ..}
        )

-- | Serialize a context-free raw op
serializeRawOp :: ClosedOp -> ByteStringL
serializeRawOp op = evalState (serializeClosedOpZip op) opZero

-- | Serialize a raw op with compression in stream context
serializeClosedOpZip :: ClosedOp -> State ClosedOp ByteStringL
serializeClosedOpZip this = state $ \prev -> let
    prev' = op prev
    typ = serializeUuidKey (reducerId prev)  zero              (reducerId this)
    obj = serializeUuidKey (objectId  prev)  (reducerId this)  (objectId  this)
    evt = serializeUuidKey (opId      prev') (objectId  this)  (opId      this')
    ref = serializeUuidKey (refId     prev') (opId      this') (refId     this')
    payloadAtoms = serializePayload (objectId this) (payload this')
    in
    ( BSLC.unwords
        $   key '*' typ
        ++  key '#' obj
        ++  key '@' evt
        ++  key ':' ref
        ++  [payloadAtoms | not $ BSL.null payloadAtoms]
    , this
    )
  where
    this' = op this
    key c u = [BSLC.cons c u | not $ BSL.null u]

-- | Serialize a reduced op with compression in stream context
serializeReducedOpZip
    :: UUID  -- ^ enclosing object
    -> Op
    -> State Op ByteStringL
serializeReducedOpZip opObject this = state $ \prev -> let
    evt = serializeUuidKey (opId  prev) opObject    (opId  this)
    ref = serializeUuidKey (refId prev) (opId this) (refId this)
    payloadAtoms = serializePayload opObject (payload this)
    in
    (   BSLC.unwords
            $   (if BSL.null evt && BSL.null ref
                    then ["@"]
                    else key '@' evt ++ key ':' ref)
            ++  [payloadAtoms | not $ BSL.null payloadAtoms]
    ,   this
    )
  where
    key c u = [BSLC.cons c u | not $ BSL.null u]

-- | Serialize a context-free atom
serializeAtom :: Atom -> ByteStringL
serializeAtom a = evalState (serializeAtomZip a) zero

-- | Serialize an atom with compression for UUID in stream context
serializeAtomZip :: Atom -> State UUID ByteStringL
serializeAtomZip = \case
    AFloat   f -> pure $ serializeFloatAtom f
    AInteger i -> pure $ serializeIntegerAtom i
    AString  s -> pure $ serializeString s
    AUuid    u -> serializeUuidAtom' u

-- | Serialize a float atom.
-- If unambiguous, i.e. contains a '.' or an 'e'/'E', the prefix '^' is skipped.
serializeFloatAtom :: Double -> ByteStringL
serializeFloatAtom f = let
    s = show f
    p = ('.' `List.elem` s || 'e' `List.elem` s || 'E' `List.elem`  s)
    b = BSLC.pack s
    in if p then b
            else BSLC.cons '^' b

-- | Serialize an integer atom.
-- Since integers are always unambiguous, the prefix '=' is always skipped.
serializeIntegerAtom :: Int64 -> ByteStringL
serializeIntegerAtom i =
    BSLC.cons '=' $ BSLC.pack (show i)

-- | Serialize a string atom
serializeString :: Text -> ByteStringL
serializeString =
    wrapSingleQuotes . escapeApostrophe . stripDoubleQuotes . Json.encode
  where
    wrapSingleQuotes = (`BSLC.snoc` '\'') . BSLC.cons '\''
    stripDoubleQuotes = BSL.init . BSL.tail
    escapeApostrophe s = let
        (s1, s2) = BSLC.break (== '\'') s
        in
        if BSL.null s2 then
            s1
        else
            s1 <> "\\'" <> escapeApostrophe (BSL.tail s2)

serializeUuidAtom' :: UUID -> State UUID ByteStringL
serializeUuidAtom' u =
    -- FIXME: Check if uuid can be unambiguously serialized and if so, skip the prefix.
    state $ \prev -> (BSLC.cons '>' $ serializeUuidAtom prev u, u)

-- | Serialize a payload in stream context
serializePayload
    :: UUID  -- ^ previous UUID (default is 'zero')
    -> [Atom]
    -> ByteStringL
serializePayload prev =
    BSLC.unwords . (`evalState` prev) . traverse serializeAtomZip

-- | Serialize a state frame
serializeStateFrame :: StateFrame -> ByteStringL
serializeStateFrame =
    serializeWireFrame . map wrapChunk . sortOn (stateType . snd) . Map.assocs
    -- TODO(2019-01-28, cblp) remove sortOn type
  where
    wrapChunk (objectId, StateChunk{..}) = Value WireReducedChunk{..}
      where
        wrcHeader = ClosedOp
            { reducerId = stateType
            , objectId
            , op = Op{opId = stateVersion, refId = zero, payload = []}
            }
        wrcBody = stateBody

-- | Serialize an object. Return object id that must be stored separately.
serializeObject :: Object a -> (UUID, ByteStringL)
serializeObject (Object oid frame) = (oid, serializeStateFrame frame)

opZero :: ClosedOp
opZero = ClosedOp
    { reducerId = zero
    , objectId  = zero
    , op        = Op{opId = zero, refId = zero, payload = []}
    }
