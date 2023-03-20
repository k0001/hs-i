-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CULong () where

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


type instance MinL CULong = MinT CULong
type instance MaxR CULong = MaxT CULong

instance forall l r.
  ( IntervalCtx CULong l r
  ) => Interval CULong l r where
  type IntervalCtx CULong l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CULong <= l
    , l <= r
    , r <= MaxT CULong )
  type MinI CULong l r = l
  type MaxI CULong l r = r

instance
  ( Interval CULong l r, InhabitedCtx CULong l r
  ) => Inhabited CULong l r where
  inhabitant = min
  from = \x -> UnsafeI x <$ guard (l <= x && x <= r)
    where l = fromInteger (L.natVal (Proxy @l)) :: CULong
          r = fromInteger (L.natVal (Proxy @r)) :: CULong

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

instance (Inhabited CULong l r) => Clamp CULong l r

instance (Inhabited CULong ld rd, Inhabited CULong lu ru, lu <= ld, rd <= ru)
  => Up CULong ld rd lu ru

instance forall l r t.
  ( Inhabited CULong l r, KnownCtx CULong l r t
  ) => Known CULong l r t where
  type KnownCtx CULong l r t = (L.KnownNat t, l <= t, t <= r)
  known' = UnsafeI . fromInteger . L.natVal

instance forall l r. (Inhabited CULong l r) => With CULong l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CULong l r, l /= r
  ) => Discrete CULong l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CULong 0 r) => Zero CULong 0 r where
  zero = UnsafeI 0

instance (Inhabited CULong l r, l <= 1, 1 <= r) => One CULong l r where
  one = UnsafeI 1

instance forall l r. (Inhabited CULong l r) => Shove CULong l r where
  shove = \x -> UnsafeI $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CULong @l @r))
          r = toInteger (unwrap (max @CULong @l @r))

