-- File generated from CIntMax.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CIntMax () where

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
  type MinBoundI CIntMax l r = l
  type MaxBoundI CIntMax l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CIntMax l r, InhabitedCtx CIntMax l r
  ) => Inhabited CIntMax l r where
  type InhabitedCtx CIntMax l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CIntMax l r, KnownCtx CIntMax t l r
  ) => Known CIntMax t l r where
  type KnownCtx CIntMax t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CIntMax l r) => With CIntMax l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited CIntMax l r, PredCtx CIntMax l r
  ) => Pred CIntMax l r where
  type PredCtx CIntMax l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CIntMax l r, SuccCtx CIntMax l r
  ) => Succ CIntMax l r where
  type SuccCtx CIntMax l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CIntMax t l r, Pred CIntMax l r, KnownPredCtx CIntMax t l r
  ) => KnownPred CIntMax t l r where
  type KnownPredCtx CIntMax t l r = t /= l
  type Pred' CIntMax t l r = t K.- K.P 1

instance
  ( Known CIntMax t l r, Succ CIntMax l r, KnownSuccCtx CIntMax t l r
  ) => KnownSucc CIntMax t l r where
  type KnownSuccCtx CIntMax t l r = t /= r
  type Succ' CIntMax t l r = t K.+ K.P 1

instance (Inhabited CIntMax l r, PlusCtx CIntMax l r) => Plus CIntMax l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CIntMax l r, Zero CIntMax l r, PlusInvCtx CIntMax l r)
  => PlusInv CIntMax l r where
  type PlusInvCtx CIntMax l r = l == K.Negate r
  plusinv = UnsafeI . negate . unwrap

instance (Inhabited CIntMax l r, MultCtx CIntMax l r) => Mult CIntMax l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CIntMax l r, MinusCtx CIntMax l r) => Minus CIntMax l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CIntMax l r, ZeroCtx CIntMax l r) => Zero CIntMax l r where
  type ZeroCtx CIntMax l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CIntMax l r, OneCtx CIntMax l r) => One CIntMax l r where
  type OneCtx CIntMax l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

