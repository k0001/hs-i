-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Int32 () where

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

type instance MinL Int32 = MinT Int32
type instance MaxR Int32 = MaxT Int32

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int32 l r
  ) => Interval Int32 l r where
  type IntervalCtx Int32 l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int32 <= l
    , l <= r
    , r <= MaxT Int32 )
  type MinI Int32 l r = l
  type MaxI Int32 l r = r
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x <= r)
    where l = fromInteger (K.integerVal (Proxy @l)) :: Int32
          r = fromInteger (K.integerVal (Proxy @r)) :: Int32
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

instance (Interval Int32 l r) => Clamp Int32 l r

instance (Interval Int32 ld rd, Interval Int32 lu ru, lu <= ld, rd <= ru)
  => Up Int32 ld rd lu ru

instance forall l r t.
  ( Interval Int32 l r, KnownCtx Int32 l r t
  ) => Known Int32 l r t where
  type KnownCtx Int32 l r t = (K.KnownInteger t, l <= t, t <= r)
  known' = unsafe . fromInteger . K.integerVal

instance forall l r. (Interval Int32 l r) => With Int32 l r where
  with x g = case K.someIntegerVal (toInteger (unwrap x)) of
    K.SomeInteger (pt :: Proxy t) ->
      fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Interval Int32 l r, l /= r) => Discrete Int32 l r where
  pred' i = unsafe (unwrap i - 1) <$ guard (min < i)
  succ' i = unsafe (unwrap i + 1) <$ guard (i < max)

instance (Zero Int32 l r, l == K.Negate r) => Negate Int32 l r where
  negate = unsafe . P.negate . unwrap

instance (Interval Int32 l r, l <= K.P 0, K.P 0 <= r) => Zero Int32 l r where
  zero = unsafe 0

instance (Interval Int32 l r, l <= K.P 1, K.P 1 <= r) => One Int32 l r where
  one = unsafe 1

instance forall l r. (Interval Int32 l r) => Shove Int32 l r where
  shove = \x -> fromMaybe (error "shove(Int32): impossible") $
                  from $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @Int32 @l @r))
          r = toInteger (unwrap (max @Int32 @l @r))

