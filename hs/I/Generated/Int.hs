-- File generated from Int.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Int () where

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

type instance MinL Int = MinT Int
type instance MaxR Int = MaxT Int

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int l r
  ) => Interval Int l r where
  type IntervalCtx Int l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int <= l
    , l <= r
    , r <= MaxT Int )
  type MinBoundI Int l r = l
  type MaxBoundI Int l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval Int l r, InhabitedCtx Int l r
  ) => Inhabited Int l r where
  type InhabitedCtx Int l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Int l r, KnownCtx Int t l r
  ) => Known Int t l r where
  type KnownCtx Int t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited Int l r) => With Int l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited Int l r, PredCtx Int l r
  ) => Pred Int l r where
  type PredCtx Int l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Int l r, SuccCtx Int l r
  ) => Succ Int l r where
  type SuccCtx Int l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Int l r, PlusCtx Int l r) => Plus Int l r where
  type PlusCtx Int l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus Int l r, Zero Int l r, PlusInvCtx Int l r)
  => PlusInv Int l r where
  type PlusInvCtx Int l r = l == K.Negate r
  plusinv = UnsafeI . negate . unwrap

instance (Inhabited Int l r, MultCtx Int l r) => Mult Int l r where
  type MultCtx Int l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited Int l r, MinusCtx Int l r) => Minus Int l r where
  type MinusCtx Int l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited Int l r, ZeroCtx Int l r) => Zero Int l r where
  type ZeroCtx Int l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited Int l r, OneCtx Int l r) => One Int l r where
  type OneCtx Int l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

