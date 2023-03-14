-- File generated from CLong.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CLong () where

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
  type InhabitedCtx CLong l r = ()
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)
  negate' x = from =<< toIntegralSized (P.negate (toInteger (unwrap x)))
  recip' _ = Nothing
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance forall t l r.
  ( Inhabited CLong l r, KnownCtx CLong t l r
  ) => Known CLong t l r where
  type KnownCtx CLong t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CLong l r) => With CLong l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited CLong l r, l /= r) => Discrete CLong l r where
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero CLong l r, l == K.Negate r) => Negate CLong l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CLong l r, l <= K.P 0, K.P 0 <= r) => Zero CLong l r where
  zero = UnsafeI 0

instance (Inhabited CLong l r, l <= K.P 1, K.P 1 <= r) => One CLong l r where
  one = UnsafeI 1

