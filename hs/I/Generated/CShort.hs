-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CShort () where

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

type instance MinL CShort = MinT CShort
type instance MaxR CShort = MaxT CShort

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CShort l r
  ) => Interval CShort l r where
  type IntervalCtx CShort l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CShort <= l
    , l <= r
    , r <= MaxT CShort )
  type MinI CShort l r = l
  type MaxI CShort l r = r
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x <= r)
    where l = fromInteger (K.integerVal (Proxy @l)) :: CShort
          r = fromInteger (K.integerVal (Proxy @r)) :: CShort
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

instance (Interval CShort l r) => Clamp CShort l r

instance (Interval CShort ld rd, Interval CShort lu ru, lu <= ld, rd <= ru)
  => Up CShort ld rd lu ru

instance forall l r t.
  ( Interval CShort l r, KnownCtx CShort l r t
  ) => Known CShort l r t where
  type KnownCtx CShort l r t = (K.KnownInteger t, l <= t, t <= r)
  known' = unsafe . fromInteger . K.integerVal

instance forall l r. (Interval CShort l r) => With CShort l r where
  with x g = case K.someIntegerVal (toInteger (unwrap x)) of
    K.SomeInteger (pt :: Proxy t) ->
      fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Interval CShort l r, l /= r) => Discrete CShort l r where
  pred' i = unsafe (unwrap i - 1) <$ guard (min < i)
  succ' i = unsafe (unwrap i + 1) <$ guard (i < max)

instance (Zero CShort l r, l == K.Negate r) => Negate CShort l r where
  negate = unsafe . P.negate . unwrap

instance (Interval CShort l r, l <= K.P 0, K.P 0 <= r) => Zero CShort l r where
  zero = unsafe 0

instance (Interval CShort l r, l <= K.P 1, K.P 1 <= r) => One CShort l r where
  one = unsafe 1

instance forall l r. (Interval CShort l r) => Shove CShort l r where
  shove = \x -> fromMaybe (error "shove(CShort): impossible") $
                  from $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CShort @l @r))
          r = toInteger (unwrap (max @CShort @l @r))

