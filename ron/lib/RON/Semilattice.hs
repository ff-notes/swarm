module RON.Semilattice (
    Semilattice,
    BoundedSemilattice,
) where

import           Data.Monoid (Monoid)
import           Data.Semigroup (Semigroup)

-- | Laws:
-- Idempotency:
class Semigroup a => Semilattice a

class (Monoid a, Semilattice a) => BoundedSemilattice a
