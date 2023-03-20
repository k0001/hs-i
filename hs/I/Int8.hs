{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Int8 () where

import Control.Monad
import Data.Bits
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

type instance MinL Int8 = MinT Int8
type instance MaxR Int8 = MaxT Int8

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int8 l r
  ) => Interval Int8 l r where
  type IntervalCtx Int8 l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int8 <= l
    , l <= r
    , r <= MaxT Int8 )
  type MinI Int8 l r = l
  type MaxI Int8 l r = r

instance
  ( Interval Int8 l r, InhabitedCtx Int8 l r
  ) => Inhabited Int8 l r where
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

  negate' x = do
    guard (unwrap x /= minBound)
    from (P.negate (unwrap x))

  a `plus'` b =
    let a' = unwrap a
        b' = unwrap b
    in case a' + b' of
         x | a' < 0 && b' < 0 && x >= 0 -> Nothing
           | a' > 0 && b' > 0 && x <  0 -> Nothing
           | otherwise -> from x

  a `mult'` b = do
    x <- toIntegralSized (toInteger (unwrap a) * toInteger (unwrap b))
    from x

  a `minus'` b =
    let a' = unwrap a
        b' = P.negate (unwrap b)
    in case a' + b' of
         x | a' < 0 && b' < 0 && x >= 0 -> Nothing
           | a' > 0 && b' > 0 && x <  0 -> Nothing
           | otherwise -> from x

  a `div'` b = do
    guard (unwrap b /= 0)
    (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
    from q

instance (Inhabited Int8 l r) => Clamp Int8 l r

instance (Inhabited Int8 ld rd, Inhabited Int8 lu ru, lu <= ld, rd <= ru)
  => Up Int8 ld rd lu ru

instance forall t l r.
  ( Inhabited Int8 l r, KnownCtx Int8 t l r
  ) => Known Int8 t l r where
  type KnownCtx Int8 t l r = (K.KnownInteger t, l <= t, t <= r)
  known' = UnsafeI . fromInteger . K.integerVal

instance forall l r. (Inhabited Int8 l r) => With Int8 l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited Int8 l r, l /= r) => Discrete Int8 l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero Int8 l r, l == K.Negate r) => Negate Int8 l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited Int8 l r, l <= K.P 0, K.P 0 <= r) => Zero Int8 l r where
  zero = UnsafeI 0

instance (Inhabited Int8 l r, l <= K.P 1, K.P 1 <= r) => One Int8 l r where
  one = UnsafeI 1

instance forall l r. (Inhabited Int8 l r) => Shove Int8 l r where
  shove = \x -> fromMaybe (error "shove(Int8): impossible") $
                  from $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @Int8 @l @r))
          r = toInteger (unwrap (max @Int8 @l @r))

