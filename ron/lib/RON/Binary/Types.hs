{-# LANGUAGE LambdaCase #-}

-- | Common types for binary format (parser and serializer)
module RON.Binary.Types where

import           RON.Prelude

type Size = Word32

-- | Data block descriptor
data Desc

    = DOpClosed
    | DOpReduced
    | DOpHeader
    | DOpQueryHeader

    | DUuidReducer
    | DUuidObject
    | DUuidOp
    | DUuidRef

    | DAtomUuidZip
    | DUuidZipObject
    | DUuidZipOp
    | DUuidZipRef

    | DAtomUuid
    | DAtomInteger
    | DAtomString
    | DAtomFloat

    deriving (Enum, Eq, Show)

-- | Does the descriptor refer to an op
descIsOp :: Desc -> Bool
descIsOp = \case
    DOpClosed       -> True
    DOpReduced      -> True
    DOpHeader       -> True
    DOpQueryHeader  -> True
    _               -> False
