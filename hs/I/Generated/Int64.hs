-- File generated from Int64.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Int64 () where

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

type instance MinL Int64 = MinT Int64
type instance MaxR Int64 = MaxT Int64

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int64 l r
  ) => Interval Int64 l r where
  type IntervalCtx Int64 l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int64 <= l
    , l <= r
    , r <= MaxT Int64 )
  type MinBoundI Int64 l r = l
  type MaxBoundI Int64 l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval Int64 l r, InhabitedCtx Int64 l r
  ) => Inhabited Int64 l r where
  type InhabitedCtx Int64 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Int64 l r, KnownCtx Int64 t l r
  ) => Known Int64 t l r where
  type KnownCtx Int64 t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited Int64 l r) => With Int64 l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited Int64 l r, PredCtx Int64 l r
  ) => Pred Int64 l r where
  type PredCtx Int64 l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Int64 l r, SuccCtx Int64 l r
  ) => Succ Int64 l r where
  type SuccCtx Int64 l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known Int64 t l r, Pred Int64 l r, KnownPredCtx Int64 t l r
  ) => KnownPred Int64 t l r where
  type KnownPredCtx Int64 t l r = t /= l
  type Pred' Int64 t l r = t K.- K.P 1

instance
  ( Known Int64 t l r, Succ Int64 l r, KnownSuccCtx Int64 t l r
  ) => KnownSucc Int64 t l r where
  type KnownSuccCtx Int64 t l r = t /= r
  type Succ' Int64 t l r = t K.+ K.P 1

instance (Inhabited Int64 l r, PlusCtx Int64 l r) => Plus Int64 l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus Int64 l r, Zero Int64 l r, PlusInvCtx Int64 l r)
  => PlusInv Int64 l r where
  type PlusInvCtx Int64 l r = l == K.Negate r
  plusinv = UnsafeI . negate . unwrap

instance (Inhabited Int64 l r, MultCtx Int64 l r) => Mult Int64 l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited Int64 l r, MinusCtx Int64 l r) => Minus Int64 l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited Int64 l r, ZeroCtx Int64 l r) => Zero Int64 l r where
  type ZeroCtx Int64 l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited Int64 l r, OneCtx Int64 l r) => One Int64 l r where
  type OneCtx Int64 l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

