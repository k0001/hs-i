-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CLong () where

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

type instance MinL CLong = MinT CLong
type instance MaxR CLong = MaxT CLong

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CLong l r
  ) => Interval CLong l r where
  type IntervalCtx CLong l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CLong <= l
    , l <= r
    , r <= MaxT CLong )
  type MinI CLong l r = l
  type MaxI CLong l r = r

instance
  ( Interval CLong l r, InhabitedCtx CLong l r
  ) => Inhabited CLong l r where
  inhabitant = min
  from = \x -> UnsafeI x <$ guard (l <= x && x <= r)
    where l = fromInteger (K.integerVal (Proxy @l)) :: CLong
          r = fromInteger (K.integerVal (Proxy @r)) :: CLong

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

instance (Inhabited CLong l r) => Clamp CLong l r

instance (Inhabited CLong ld rd, Inhabited CLong lu ru, lu <= ld, rd <= ru)
  => Up CLong ld rd lu ru

instance forall l r t.
  ( Inhabited CLong l r, KnownCtx CLong l r t
  ) => Known CLong l r t where
  type KnownCtx CLong l r t = (K.KnownInteger t, l <= t, t <= r)
  known' = UnsafeI . fromInteger . K.integerVal

instance forall l r. (Inhabited CLong l r) => With CLong l r where
  with x g = case K.someIntegerVal (toInteger (unwrap x)) of
    K.SomeInteger (pt :: Proxy t) ->
      fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited CLong l r, l /= r) => Discrete CLong l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero CLong l r, l == K.Negate r) => Negate CLong l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CLong l r, l <= K.P 0, K.P 0 <= r) => Zero CLong l r where
  zero = UnsafeI 0

instance (Inhabited CLong l r, l <= K.P 1, K.P 1 <= r) => One CLong l r where
  one = UnsafeI 1

instance forall l r. (Inhabited CLong l r) => Shove CLong l r where
  shove = \x -> fromMaybe (error "shove(CLong): impossible") $
                  from $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CLong @l @r))
          r = toInteger (unwrap (max @CLong @l @r))

