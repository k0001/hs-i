-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUIntMax () where

import Control.Monad
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Data.Word
import Data.Type.Ord
import Foreign.C.Types
import GHC.TypeLits qualified as L
import KindInteger (type (/=))
import Prelude hiding (min, max, div)

import I.Internal

--------------------------------------------------------------------------------

-- | This is so that GHC doesn't complain about the unused modules,
-- which we import here so that `genmodules.sh` doesn't have to add it
-- to the generated modules.
_ignore :: (CSize, Word)
_ignore = (0, 0)

--------------------------------------------------------------------------------


type instance MinL CUIntMax = MinT CUIntMax
type instance MaxR CUIntMax = MaxT CUIntMax

instance forall l r.
  ( IntervalCtx CUIntMax l r
  ) => Interval CUIntMax l r where
  type IntervalCtx CUIntMax l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUIntMax <= l
    , l <= r
    , r <= MaxT CUIntMax )
  type MinI CUIntMax l r = l
  type MaxI CUIntMax l r = r

instance
  ( Interval CUIntMax l r, InhabitedCtx CUIntMax l r
  ) => Inhabited CUIntMax l r where
  inhabitant = min
  from = \x -> UnsafeI x <$ guard (l <= x && x <= r)
    where l = fromInteger (L.natVal (Proxy @l)) :: CUIntMax
          r = fromInteger (L.natVal (Proxy @r)) :: CUIntMax

  (unwrap -> a) `plus'` (unwrap -> b) = do
    guard (b <= maxBound - a)
    from (a + b)

  (unwrap -> a) `mult'` (unwrap -> b) = do
    guard (b == 0 || a <= maxBound `quot` b)
    from (a * b)

  (unwrap -> a) `minus'` (unwrap -> b) = do
    guard (b <= a)
    from (a - b)

  (unwrap -> a) `div'` (unwrap -> b) = do
    guard (b /= 0)
    let (q, m) = divMod a b
    guard (m == 0)
    from q

instance (Inhabited CUIntMax l r) => Clamp CUIntMax l r

instance (Inhabited CUIntMax ld rd, Inhabited CUIntMax lu ru, lu <= ld, rd <= ru)
  => Up CUIntMax ld rd lu ru

instance forall l r t.
  ( Inhabited CUIntMax l r, KnownCtx CUIntMax l r t
  ) => Known CUIntMax l r t where
  type KnownCtx CUIntMax l r t = (L.KnownNat t, l <= t, t <= r)
  known' = UnsafeI . fromInteger . L.natVal

instance forall l r. (Inhabited CUIntMax l r) => With CUIntMax l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUIntMax l r, l /= r
  ) => Discrete CUIntMax l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CUIntMax 0 r) => Zero CUIntMax 0 r where
  zero = UnsafeI 0

instance (Inhabited CUIntMax l r, l <= 1, 1 <= r) => One CUIntMax l r where
  one = UnsafeI 1

instance forall l r. (Inhabited CUIntMax l r) => Shove CUIntMax l r where
  shove = \x -> UnsafeI $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CUIntMax @l @r))
          r = toInteger (unwrap (max @CUIntMax @l @r))

