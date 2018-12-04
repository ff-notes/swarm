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

import           RON.Internal.Prelude

import           Control.Monad.State.Strict (State, evalState, runState, state)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Traversable (for)

import           RON.Text.Serialize.UUID (serializeUuid, serializeUuidAtom,
                                          serializeUuidKey)
import           RON.Types (Atom (AFloat, AInteger, AString, AUuid),
                            Object (..), Op (..), RawOp (..), StateChunk (..),
                            StateFrame, WireChunk (Query, Raw, Value),
                            WireFrame, WireReducedChunk (..))
import           RON.UUID (UUID, zero)

-- | Serialize a common frame
serializeWireFrame :: WireFrame -> ByteStringL
serializeWireFrame chunks
    = (`BSLC.snoc` '.')
    . mconcat
    . (`evalState` opZero)
    $ traverse serializeChunk chunks

-- | Serialize a sequence of common frames
serializeWireFrames :: [WireFrame] -> ByteStringL
serializeWireFrames = foldMap serializeWireFrame

-- | Serialize a common chunk
serializeChunk :: WireChunk -> State RawOp ByteStringL
serializeChunk = \case
    Raw op      -> (<> " ;\n") <$> serializeRawOpZip op
    Value chunk -> serializeReducedChunk False chunk
    Query chunk -> serializeReducedChunk True  chunk

-- | Serialize a reduced chunk
serializeReducedChunk :: Bool -> WireReducedChunk -> State RawOp ByteStringL
serializeReducedChunk isQuery WireReducedChunk{wrcHeader, wrcBody} =
    BSLC.unlines <$> liftA2 (:) serializeHeader serializeBody
  where
    serializeHeader = do
        h <- serializeRawOpZip wrcHeader
        pure $ BSLC.unwords [h, if isQuery then "?" else "!"]
    serializeBody = state $ \RawOp{op = opBefore, ..} -> let
        (body, opAfter) =
            (`runState` opBefore) $
            for wrcBody $
            fmap ("\t" <>) . serializeReducedOpZip opObject
        in
        ( body
        , RawOp{op = opAfter, ..}
        )

-- | Serialize a context-free raw op
serializeRawOp :: RawOp -> ByteStringL
serializeRawOp op = evalState (serializeRawOpZip op) opZero

-- | Serialize a raw op with compression in stream context
serializeRawOpZip :: RawOp -> State RawOp ByteStringL
serializeRawOpZip this = state $ \prev -> let
    prev' = op prev
    typ = serializeUuidKey (opType   prev)  zero             (opType   this)
    obj = serializeUuidKey (opObject prev)  (opType   this)  (opObject this)
    evt = serializeUuidKey (opEvent  prev') (opObject this)  (opEvent  this')
    ref = serializeUuidKey (opRef    prev') (opEvent  this') (opRef    this')
    payload = serializePayload (opObject this) (opPayload this')
    in
    ( BSLC.unwords
        $   key '*' typ
        ++  key '#' obj
        ++  key '@' evt
        ++  key ':' ref
        ++  [payload | not $ BSL.null payload]
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
    evt = serializeUuidKey (opEvent  prev) opObject       (opEvent this)
    ref = serializeUuidKey (opRef    prev) (opEvent this) (opRef   this)
    payload = serializePayload opObject (opPayload this)
    in
    (   BSLC.unwords
            $   (if BSL.null evt && BSL.null ref
                    then ["@"]
                    else key '@' evt ++ key ':' ref)
            ++  [payload | not $ BSL.null payload]
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
    AFloat   f -> pure $ BSLC.cons '^' $ BSLC.pack (show f)
    AInteger i -> pure $ BSLC.cons '=' $ BSLC.pack (show i)
    AString  s -> pure $ serializeString s
    AUuid    u ->
        state $ \prev -> (BSLC.cons '>' $ serializeUuidAtom prev u, u)

-- | Serialize a string atom
serializeString :: Text -> ByteStringL
serializeString =
    wrapSingleQuotes . escapeApostrophe . stripDoubleQuotes . Json.encode
  where
    wrapSingleQuotes = (`BSLC.snoc` '\'') . BSLC.cons '\''
    stripDoubleQuotes = BSL.init . BSL.tail
    escapeApostrophe s = let
        (s1, s2) = BSLC.break (== '\'') s
        in if BSL.null s2 then s1 else s1 <> BSL.tail s2

-- | Serialize a payload in stream context
serializePayload
    :: UUID  -- ^ previous UUID (default is 'zero')
    -> [Atom]
    -> ByteStringL
serializePayload prev =
    BSLC.unwords . (`evalState` prev) . traverse serializeAtomZip

-- | Serialize a state frame
serializeStateFrame :: StateFrame -> ByteStringL
serializeStateFrame = serializeWireFrame . map wrapChunk . Map.assocs where
    wrapChunk ((opType, opObject), StateChunk{..}) = Value WireReducedChunk{..}
      where
        wrcHeader = RawOp{op = Op{opRef = zero, opPayload = [], ..}, ..}
        wrcBody = stateBody
        opEvent = stateVersion

-- | Serialize an object. Return object id that must be stored separately.
serializeObject :: Object a -> (UUID, ByteStringL)
serializeObject (Object oid frame) = (oid, serializeStateFrame frame)

opZero :: RawOp
opZero = RawOp
    { opType   = zero
    , opObject = zero
    , op       = Op{opEvent = zero, opRef = zero, opPayload = []}
    }
