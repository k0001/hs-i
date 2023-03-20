-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CIntMax () where

import Control.Monad
import Data.Constraint
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Type.Ord
import Foreign.C.Types
import KindInteger (type (/=), type (==))
import KindInteger qualified as K
import Prelude hiding (min, max, div)
import Prelude qualified as P

import I.Internal

--------------------------------------------------------------------------------

-- | This is so that GHC doesn't complain about the unused modules,
-- which we import here so that `genmodules.sh` doesn't have to add it
-- to the generated modules.
_ignore :: (CSize, Int)
_ignore = (0, 0)

--------------------------------------------------------------------------------

type instance MinL CIntMax = MinT CIntMax
type instance MaxR CIntMax = MaxT CIntMax

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CIntMax l r
  ) => Interval CIntMax l r where
  type IntervalCtx CIntMax l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CIntMax <= l
    , l <= r
    , r <= MaxT CIntMax )
  type MinI CIntMax l r = l
  type MaxI CIntMax l r = r

instance
  ( Interval CIntMax l r, InhabitedCtx CIntMax l r
  ) => Inhabited CIntMax l r where
  inhabitant = min
  from = \x -> UnsafeI x <$ guard (l <= x && x <= r)
    where l = fromInteger (K.integerVal (Proxy @l)) :: CIntMax
          r = fromInteger (K.integerVal (Proxy @r)) :: CIntMax

  negate' (unwrap -> x) = do
    guard (x /= minBound)
    from (P.negate x)

  (unwrap -> a) `plus'` (unwrap -> b)
    | b > 0 && a > maxBound - b = Nothing
    | b < 0 && a < minBound - b = Nothing
    | otherwise                 = from (a + b)

  (unwrap -> a) `mult'` (unwrap -> b) = do
    guard $ case a <= 0 of
      True  | b <= 0    -> a == 0 || b >= (maxBound `quot` a)
            | otherwise -> a >= (minBound `quot` b)
      False | b <= 0    -> b >= (minBound `quot` a)
            | otherwise -> a <= (maxBound `quot` b)
    from (a * b)

  (unwrap -> a) `minus'` (unwrap -> b)
    | b > 0 && a < minBound + b = Nothing
    | b < 0 && a > maxBound + b = Nothing
    | otherwise                 = from (a - b)

  (unwrap -> a) `div'` (unwrap -> b) = do
    guard (b /= 0 && (b /= -1 || a /= minBound))
    let (q, m) = divMod a b
    guard (m == 0)
    from q

instance (Inhabited CIntMax l r) => Clamp CIntMax l r

instance (Inhabited CIntMax ld rd, Inhabited CIntMax lu ru, lu <= ld, rd <= ru)
  => Up CIntMax ld rd lu ru

instance forall l r t.
  ( Inhabited CIntMax l r, KnownCtx CIntMax l r t
  ) => Known CIntMax l r t where
  type KnownCtx CIntMax l r t = (K.KnownInteger t, l <= t, t <= r)
  known' = UnsafeI . fromInteger . K.integerVal

instance forall l r. (Inhabited CIntMax l r) => With CIntMax l r where
  with x g = case K.someIntegerVal (toInteger (unwrap x)) of
    K.SomeInteger (pt :: Proxy t) ->
      fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited CIntMax l r, l /= r) => Discrete CIntMax l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero CIntMax l r, l == K.Negate r) => Negate CIntMax l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CIntMax l r, l <= K.P 0, K.P 0 <= r) => Zero CIntMax l r where
  zero = UnsafeI 0

instance (Inhabited CIntMax l r, l <= K.P 1, K.P 1 <= r) => One CIntMax l r where
  one = UnsafeI 1

instance forall l r. (Inhabited CIntMax l r) => Shove CIntMax l r where
  shove = \x -> fromMaybe (error "shove(CIntMax): impossible") $
                  from $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CIntMax @l @r))
          r = toInteger (unwrap (max @CIntMax @l @r))

