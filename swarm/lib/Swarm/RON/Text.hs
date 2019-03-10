module Swarm.RON.Text (
    TextFrame (..),
    TextFrameC,
) where

import           Foreign (ForeignPtr)

-- | Tag for 'Ptr' to @ron::TextFrame@
data TextFrameC

-- | Equivalent of @ron::TextFrame@
newtype TextFrame = TextFrame (ForeignPtr TextFrameC)
