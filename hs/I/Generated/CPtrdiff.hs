-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CPtrdiff () where

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

type instance MinL CPtrdiff = MinT CPtrdiff
type instance MaxR CPtrdiff = MaxT CPtrdiff

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CPtrdiff l r
  ) => Interval CPtrdiff l r where
  type IntervalCtx CPtrdiff l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CPtrdiff <= l
    , l <= r
    , r <= MaxT CPtrdiff )
  type MinI CPtrdiff l r = l
  type MaxI CPtrdiff l r = r

instance
  ( Interval CPtrdiff l r, InhabitedCtx CPtrdiff l r
  ) => Inhabited CPtrdiff l r where
  inhabitant = min
  from = \x -> UnsafeI x <$ guard (l <= x && x <= r)
    where l = fromInteger (K.integerVal (Proxy @l)) :: CPtrdiff
          r = fromInteger (K.integerVal (Proxy @r)) :: CPtrdiff

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

instance (Inhabited CPtrdiff l r) => Clamp CPtrdiff l r

instance (Inhabited CPtrdiff ld rd, Inhabited CPtrdiff lu ru, lu <= ld, rd <= ru)
  => Up CPtrdiff ld rd lu ru

instance forall l r t.
  ( Inhabited CPtrdiff l r, KnownCtx CPtrdiff l r t
  ) => Known CPtrdiff l r t where
  type KnownCtx CPtrdiff l r t = (K.KnownInteger t, l <= t, t <= r)
  known' = UnsafeI . fromInteger . K.integerVal

instance forall l r. (Inhabited CPtrdiff l r) => With CPtrdiff l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited CPtrdiff l r, l /= r) => Discrete CPtrdiff l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero CPtrdiff l r, l == K.Negate r) => Negate CPtrdiff l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CPtrdiff l r, l <= K.P 0, K.P 0 <= r) => Zero CPtrdiff l r where
  zero = UnsafeI 0

instance (Inhabited CPtrdiff l r, l <= K.P 1, K.P 1 <= r) => One CPtrdiff l r where
  one = UnsafeI 1

instance forall l r. (Inhabited CPtrdiff l r) => Shove CPtrdiff l r where
  shove = \x -> fromMaybe (error "shove(CPtrdiff): impossible") $
                  from $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CPtrdiff @l @r))
          r = toInteger (unwrap (max @CPtrdiff @l @r))

