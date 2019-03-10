{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Swarm.DB.Replica (
    Replica (get),
) where

import           Control.Exception (mask_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Foreign (FinalizerPtr, ForeignPtr, Ptr, allocaArray,
                          newForeignPtr, peekElemOff, wordPtrToPtr)
import           Language.C.Inline.Context (ctxTypesTable)
import qualified Language.C.Inline.Cpp as Cpp
import           Language.C.Types (TypeSpecifier (TypeName))

import           RON.UUID (UUID (UUID))

import           Swarm.RON.Status (Status (Status), StatusC)
import qualified Swarm.RON.Status as Status
import           Swarm.RON.Text (TextFrame (TextFrame), TextFrameC)

-- | Tag for 'Ptr' to @ron::Replica<TextFrame>@
data TextReplicaC

-- | Equivalent of @ron::Replica<TextFrame>@
newtype TextReplica = TextReplica (Ptr TextReplicaC)

$(Cpp.context
    $   Cpp.cppCtx
    <>  Cpp.fptrCtx
    <>  mempty
        { ctxTypesTable = Map.fromList
            [ (TypeName "Status"     , [t| StatusC      |])
            , (TypeName "TextFrame"  , [t| TextFrameC   |])
            , (TypeName "TextReplica", [t| TextReplicaC |])
            ]
        }
    )
Cpp.include "<cstddef>"
Cpp.include "<swarm/db/replica.hpp>"
Cpp.include "<swarm/ron/status.hpp>"
Cpp.include "<swarm/ron/text.hpp>"
Cpp.verbatim "typedef ron::Status Status;"
Cpp.verbatim "typedef ron::TextFrame TextFrame;"
Cpp.verbatim "typedef ron::Replica<ron::TextFrame> TextReplica;"

-- | @class ron::Replica<>@
class Replica replica frame | frame -> replica, replica -> frame where

    -- | @ron::Replica::Get()@
    get :: UUID -> replica -> IO (Either Status frame)

instance Replica TextReplica TextFrame where

    get (UUID x y) (TextReplica replicaP) = do
        frameFP <- newTextFrame
        status <- do
            statusFP <- mask_ $ do
                statusP <- [Cpp.exp| Status * {
                    new Status(
                        $(TextReplica * replicaP)
                        ->Get(
                            * $fptr-ptr:(TextFrame * frameFP),
                            {$(uint64_t x), $(uint64_t y)}
                        )
                    )
                } |]
                newForeignPtr deleteStatus statusP
            allocaArray 4 $ \arena -> do
                [Cpp.block| void {
                    uint64_t * const arena = $(uint64_t * arena);
                    uint64_t & x   = arena[0];
                    uint64_t & y   = arena[1];
                    uint64_t & ptr = arena[2];
                    uint64_t & len = arena[3];
                    Status & status = * $fptr-ptr:(Status * statusFP);
                    x = uint64_t(status.code().value());
                    y = uint64_t(status.code().origin());
                    ptr = uintptr_t(status.comment().data());
                    len = status.comment().length();
                } |]
                code <- UUID <$> peekElemOff arena 0 <*> peekElemOff arena 1
                ptr <- wordPtrToPtr . fromIntegral <$> peekElemOff arena 2
                len <- fromIntegral <$> peekElemOff arena 3
                comment <- BS.packCStringLen (ptr, len)
                pure Status{code, comment}
        pure $ case status of
            Status code _ | code == Status.ok -> Right $ TextFrame frameFP
            _                                 -> Left status

newTextFrame :: IO (ForeignPtr TextFrameC)
newTextFrame = mask_ $ do
    p <- [Cpp.exp| TextFrame * { new TextFrame } |]
    newForeignPtr deleteTextFrame p

foreign import ccall "&deleteStatus" deleteStatus :: FinalizerPtr StatusC

foreign import ccall "&deleteTextFrame" deleteTextFrame
    :: FinalizerPtr TextFrameC
