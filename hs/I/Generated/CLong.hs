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
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CLong l r, InhabitedCtx CLong l r
  ) => Inhabited CLong l r where
  type InhabitedCtx CLong l r = ()
  inhabitant = min

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

instance
  ( Inhabited CLong l r, PredCtx CLong l r
  ) => Pred CLong l r where
  type PredCtx CLong l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CLong l r, SuccCtx CLong l r
  ) => Succ CLong l r where
  type SuccCtx CLong l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CLong l r, PlusCtx CLong l r) => Plus CLong l r where
  type PlusCtx CLong l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CLong l r, Zero CLong l r, MayNegateCtx CLong l r)
  => MayNegate CLong l r where
  type MayNegateCtx CLong l r = l < K.P 0
  negate' x = from =<< toIntegralSized (P.negate (toInteger (unwrap x)))

instance (MayNegate CLong l r, NegateCtx CLong l r)
  => Negate CLong l r where
  type NegateCtx CLong l r = l == K.Negate r
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CLong l r, MultCtx CLong l r) => Mult CLong l r where
  type MultCtx CLong l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CLong l r, MinusCtx CLong l r) => Minus CLong l r where
  type MinusCtx CLong l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CLong l r, ZeroCtx CLong l r) => Zero CLong l r where
  type ZeroCtx CLong l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CLong l r, OneCtx CLong l r) => One CLong l r where
  type OneCtx CLong l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

