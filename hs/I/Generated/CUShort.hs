-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUShort () where

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


type instance MinL CUShort = MinT CUShort
type instance MaxR CUShort = MaxT CUShort

instance forall l r.
  ( IntervalCtx CUShort l r
  ) => Interval CUShort l r where
  type IntervalCtx CUShort l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUShort <= l
    , l <= r
    , r <= MaxT CUShort )
  type MinI CUShort l r = l
  type MaxI CUShort l r = r
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x <= r)
    where l = fromInteger (L.natVal (Proxy @l)) :: CUShort
          r = fromInteger (L.natVal (Proxy @r)) :: CUShort
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

instance (Interval CUShort l r) => Clamp CUShort l r

instance (Interval CUShort ld rd, Interval CUShort lu ru, lu <= ld, rd <= ru)
  => Up CUShort ld rd lu ru

instance forall l r t.
  ( Interval CUShort l r, KnownCtx CUShort l r t
  ) => Known CUShort l r t where
  type KnownCtx CUShort l r t = (L.KnownNat t, l <= t, t <= r)
  known' = unsafe . fromInteger . L.natVal

instance forall l r. (Interval CUShort l r) => With CUShort l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance (Interval CUShort l r, l /= r) => Discrete CUShort l r where
  pred' i = unsafe (unwrap i - 1) <$ guard (min < i)
  succ' i = unsafe (unwrap i + 1) <$ guard (i < max)

instance (Interval CUShort 0 r) => Zero CUShort 0 r where
  zero = unsafe 0

instance (Interval CUShort l r, l <= 1, 1 <= r) => One CUShort l r where
  one = unsafe 1

instance forall l r. (Interval CUShort l r) => Shove CUShort l r where
  shove = \x -> unsafe $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CUShort @l @r))
          r = toInteger (unwrap (max @CUShort @l @r))

