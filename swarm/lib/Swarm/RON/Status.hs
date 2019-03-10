{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.RON.Status (
    Status (..),
    StatusC,
    ok,
) where

import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Language.C.Inline.Context (ctxTypesTable)
import qualified Language.C.Inline.Cpp as Cpp
import           Language.C.Types (TypeSpecifier (TypeName))

import           RON.UUID (UUID (UUID))

$(Cpp.context
    $ Cpp.cppCtx
    <> mempty{ctxTypesTable = Map.singleton (TypeName "uint64_t") [t| Word64 |]}
    )
Cpp.include "<cstdint>"
Cpp.include "<swarm/ron/status.hpp>"

-- | Tag for 'Ptr' to @ron::Status@
data StatusC

-- | Equivalent of @ron::Status@
data Status = Status{code :: UUID, comment :: ByteString}

ok :: UUID
ok = UUID
    [Cpp.pure| uint64_t { uint64_t(ron::Status::OK.code().value ()) } |]
    [Cpp.pure| uint64_t { uint64_t(ron::Status::OK.code().origin()) } |]

--     | ENDOFFRAME
--     | NOT_IMPLEMENTED
--     | NOT_FOUND
--     | BAD_STATE
--     | BADARGS
--     | BADSYNTAX
--     | DB_FAIL
--     | IOFAIL
--     | BADFRAME
--     | BADID
--     | BADREF
--     | BADVALUE

--     | NOTYPE
--     | NOTOPEN

--     | CHAINBREAK
--     | HASHBREAK
--     | TREEBREAK
--     | CAUSEBREAK

--     | TREEGAP
--     | YARNGAP

--     | REPEAT
--     | REORDER
