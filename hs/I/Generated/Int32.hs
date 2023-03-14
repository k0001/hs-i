-- File generated from Int32.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Int32 () where

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

type instance MinL Int32 = MinT Int32
type instance MaxR Int32 = MaxT Int32

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int32 l r
  ) => Interval Int32 l r where
  type IntervalCtx Int32 l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int32 <= l
    , l <= r
    , r <= MaxT Int32 )
  type MinBoundI Int32 l r = l
  type MaxBoundI Int32 l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval Int32 l r, InhabitedCtx Int32 l r
  ) => Inhabited Int32 l r where
  type InhabitedCtx Int32 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Int32 l r, KnownCtx Int32 t l r
  ) => Known Int32 t l r where
  type KnownCtx Int32 t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited Int32 l r) => With Int32 l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited Int32 l r, PredCtx Int32 l r
  ) => Pred Int32 l r where
  type PredCtx Int32 l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Int32 l r, SuccCtx Int32 l r
  ) => Succ Int32 l r where
  type SuccCtx Int32 l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known Int32 t l r, Pred Int32 l r, KnownPredCtx Int32 t l r
  ) => KnownPred Int32 t l r where
  type KnownPredCtx Int32 t l r = t /= l
  type Pred' Int32 t l r = t K.- K.P 1
instance
  ( Known Int32 t l r, Succ Int32 l r, KnownSuccCtx Int32 t l r
  ) => KnownSucc Int32 t l r where
  type KnownSuccCtx Int32 t l r = t /= r
  type Succ' Int32 t l r = t K.+ K.P 1

instance (Inhabited Int32 l r, PlusCtx Int32 l r) => Plus Int32 l r
instance (Plus Int32 l r, Zero Int32 l r, PlusInvCtx Int32 l r)
  => PlusInv Int32 l r
instance (Inhabited Int32 l r, MultCtx Int32 l r) => Mult Int32 l r
instance (Inhabited Int32 l r, MinusCtx Int32 l r) => Minus Int32 l r
instance (Inhabited Int32 l r, ZeroCtx Int32 l r) => Zero Int32 l r where
  type ZeroCtx Int32 l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0
instance (Inhabited Int32 l r, OneCtx Int32 l r) => One Int32 l r where
  type OneCtx Int32 l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

