-- File generated from CInt.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CInt () where

import Control.Monad
import Data.Constraint
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Type.Ord
import Foreign.C.Types
import KindInteger (type (/=))
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

type instance MinL CInt = MinT CInt
type instance MaxR CInt = MaxT CInt

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CInt l r
  ) => Interval CInt l r where
  type IntervalCtx CInt l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CInt <= l
    , l <= r
    , r <= MaxT CInt )
  type MinBoundI CInt l r = l
  type MaxBoundI CInt l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CInt l r, InhabitedCtx CInt l r
  ) => Inhabited CInt l r where
  type InhabitedCtx CInt l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CInt l r, KnownCtx CInt t l r
  ) => Known CInt t l r where
  type KnownCtx CInt t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CInt l r) => With CInt l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited CInt l r, PredCtx CInt l r
  ) => Pred CInt l r where
  type PredCtx CInt l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CInt l r, SuccCtx CInt l r
  ) => Succ CInt l r where
  type SuccCtx CInt l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CInt t l r, Pred CInt l r, KnownPredCtx CInt t l r
  ) => KnownPred CInt t l r where
  type KnownPredCtx CInt t l r = t /= l
  type Pred' CInt t l r = t K.- K.P 1
instance
  ( Known CInt t l r, Succ CInt l r, KnownSuccCtx CInt t l r
  ) => KnownSucc CInt t l r where
  type KnownSuccCtx CInt t l r = t /= r
  type Succ' CInt t l r = t K.+ K.P 1

instance (Inhabited CInt l r, PlusCtx CInt l r) => Plus CInt l r
instance (Plus CInt l r, Zero CInt l r, PlusInvCtx CInt l r)
  => PlusInv CInt l r
instance (Inhabited CInt l r, MultCtx CInt l r) => Mult CInt l r
instance (Inhabited CInt l r, MinusCtx CInt l r) => Minus CInt l r
instance (Inhabited CInt l r, ZeroCtx CInt l r) => Zero CInt l r where
  type ZeroCtx CInt l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0
instance (Inhabited CInt l r, OneCtx CInt l r) => One CInt l r where
  type OneCtx CInt l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

