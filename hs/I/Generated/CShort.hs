-- File generated from CShort.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CShort () where

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
  type MinBoundI CShort l r = l
  type MaxBoundI CShort l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CShort l r, InhabitedCtx CShort l r
  ) => Inhabited CShort l r where
  type InhabitedCtx CShort l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CShort l r, KnownCtx CShort t l r
  ) => Known CShort t l r where
  type KnownCtx CShort t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CShort l r) => With CShort l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited CShort l r, PredCtx CShort l r
  ) => Pred CShort l r where
  type PredCtx CShort l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CShort l r, SuccCtx CShort l r
  ) => Succ CShort l r where
  type SuccCtx CShort l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CShort l r, PlusCtx CShort l r) => Plus CShort l r where
  type PlusCtx CShort l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CShort l r, Zero CShort l r, PlusInvCtx CShort l r)
  => PlusInv CShort l r where
  type PlusInvCtx CShort l r = l == K.Negate r
  plusinv = UnsafeI . negate . unwrap

instance (Inhabited CShort l r, MultCtx CShort l r) => Mult CShort l r where
  type MultCtx CShort l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CShort l r, MinusCtx CShort l r) => Minus CShort l r where
  type MinusCtx CShort l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CShort l r, ZeroCtx CShort l r) => Zero CShort l r where
  type ZeroCtx CShort l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CShort l r, OneCtx CShort l r) => One CShort l r where
  type OneCtx CShort l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

