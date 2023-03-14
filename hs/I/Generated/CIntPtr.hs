-- File generated from CIntPtr.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CIntPtr () where

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

type instance MinL CIntPtr = MinT CIntPtr
type instance MaxR CIntPtr = MaxT CIntPtr

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CIntPtr l r
  ) => Interval CIntPtr l r where
  type IntervalCtx CIntPtr l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CIntPtr <= l
    , l <= r
    , r <= MaxT CIntPtr )
  type MinBoundI CIntPtr l r = l
  type MaxBoundI CIntPtr l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CIntPtr l r, InhabitedCtx CIntPtr l r
  ) => Inhabited CIntPtr l r where
  type InhabitedCtx CIntPtr l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CIntPtr l r, KnownCtx CIntPtr t l r
  ) => Known CIntPtr t l r where
  type KnownCtx CIntPtr t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CIntPtr l r) => With CIntPtr l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited CIntPtr l r, PredCtx CIntPtr l r
  ) => Pred CIntPtr l r where
  type PredCtx CIntPtr l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CIntPtr l r, SuccCtx CIntPtr l r
  ) => Succ CIntPtr l r where
  type SuccCtx CIntPtr l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CIntPtr t l r, Pred CIntPtr l r, KnownPredCtx CIntPtr t l r
  ) => KnownPred CIntPtr t l r where
  type KnownPredCtx CIntPtr t l r = t /= l
  type Pred' CIntPtr t l r = t K.- K.P 1

instance
  ( Known CIntPtr t l r, Succ CIntPtr l r, KnownSuccCtx CIntPtr t l r
  ) => KnownSucc CIntPtr t l r where
  type KnownSuccCtx CIntPtr t l r = t /= r
  type Succ' CIntPtr t l r = t K.+ K.P 1

instance (Inhabited CIntPtr l r, PlusCtx CIntPtr l r) => Plus CIntPtr l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CIntPtr l r, Zero CIntPtr l r, PlusInvCtx CIntPtr l r)
  => PlusInv CIntPtr l r where
  type PlusInvCtx CIntPtr l r = l == K.Negate r
  plusinv = UnsafeI . negate . unwrap

instance (Inhabited CIntPtr l r, MultCtx CIntPtr l r) => Mult CIntPtr l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CIntPtr l r, MinusCtx CIntPtr l r) => Minus CIntPtr l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CIntPtr l r, ZeroCtx CIntPtr l r) => Zero CIntPtr l r where
  type ZeroCtx CIntPtr l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CIntPtr l r, OneCtx CIntPtr l r) => One CIntPtr l r where
  type OneCtx CIntPtr l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

