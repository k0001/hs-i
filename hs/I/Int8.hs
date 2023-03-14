{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Int8 () where

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

type instance MinL Int8 = MinT Int8
type instance MaxR Int8 = MaxT Int8

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int8 l r
  ) => Interval Int8 l r where
  type IntervalCtx Int8 l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int8 <= l
    , l <= r
    , r <= MaxT Int8 )
  type MinBoundI Int8 l r = l
  type MaxBoundI Int8 l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval Int8 l r, InhabitedCtx Int8 l r
  ) => Inhabited Int8 l r where
  type InhabitedCtx Int8 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Int8 l r, KnownCtx Int8 t l r
  ) => Known Int8 t l r where
  type KnownCtx Int8 t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited Int8 l r) => With Int8 l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited Int8 l r, PredCtx Int8 l r
  ) => Pred Int8 l r where
  type PredCtx Int8 l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Int8 l r, SuccCtx Int8 l r
  ) => Succ Int8 l r where
  type SuccCtx Int8 l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known Int8 t l r, Pred Int8 l r, KnownPredCtx Int8 t l r
  ) => KnownPred Int8 t l r where
  type KnownPredCtx Int8 t l r = t /= l
  type Pred' Int8 t l r = t K.- K.P 1
instance
  ( Known Int8 t l r, Succ Int8 l r, KnownSuccCtx Int8 t l r
  ) => KnownSucc Int8 t l r where
  type KnownSuccCtx Int8 t l r = t /= r
  type Succ' Int8 t l r = t K.+ K.P 1

instance (Inhabited Int8 l r, PlusCtx Int8 l r) => Plus Int8 l r
instance (Plus Int8 l r, Zero Int8 l r, PlusInvCtx Int8 l r)
  => PlusInv Int8 l r
instance (Inhabited Int8 l r, MultCtx Int8 l r) => Mult Int8 l r
instance (Inhabited Int8 l r, MinusCtx Int8 l r) => Minus Int8 l r
instance (Inhabited Int8 l r, ZeroCtx Int8 l r) => Zero Int8 l r where
  type ZeroCtx Int8 l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0
instance (Inhabited Int8 l r, OneCtx Int8 l r) => One Int8 l r where
  type OneCtx Int8 l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

