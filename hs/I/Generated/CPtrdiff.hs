-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CPtrdiff () where

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

  a `mult'` b =
     from =<< toIntegralSized (toInteger (unwrap a) *
                               toInteger (unwrap b))

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

instance (Inhabited CPtrdiff l r) => Clamp CPtrdiff l r

instance (Inhabited CPtrdiff ld rd, Inhabited CPtrdiff lu ru, lu <= ld, rd <= ru)
  => Up CPtrdiff ld rd lu ru

instance forall t l r.
  ( Inhabited CPtrdiff l r, KnownCtx CPtrdiff t l r
  ) => Known CPtrdiff t l r where
  type KnownCtx CPtrdiff t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

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

