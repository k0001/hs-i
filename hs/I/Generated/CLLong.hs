-- File generated from CLLong.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CLLong () where

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

type instance MinL CLLong = MinT CLLong
type instance MaxR CLLong = MaxT CLLong

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CLLong l r
  ) => Interval CLLong l r where
  type IntervalCtx CLLong l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CLLong <= l
    , l <= r
    , r <= MaxT CLLong )
  type MinBoundI CLLong l r = l
  type MaxBoundI CLLong l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CLLong l r, InhabitedCtx CLLong l r
  ) => Inhabited CLLong l r where
  type InhabitedCtx CLLong l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CLLong l r, KnownCtx CLLong t l r
  ) => Known CLLong t l r where
  type KnownCtx CLLong t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CLLong l r) => With CLLong l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited CLLong l r, PredCtx CLLong l r
  ) => Pred CLLong l r where
  type PredCtx CLLong l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CLLong l r, SuccCtx CLLong l r
  ) => Succ CLLong l r where
  type SuccCtx CLLong l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CLLong t l r, Pred CLLong l r, KnownPredCtx CLLong t l r
  ) => KnownPred CLLong t l r where
  type KnownPredCtx CLLong t l r = t /= l
  type Pred' CLLong t l r = t K.- K.P 1

instance
  ( Known CLLong t l r, Succ CLLong l r, KnownSuccCtx CLLong t l r
  ) => KnownSucc CLLong t l r where
  type KnownSuccCtx CLLong t l r = t /= r
  type Succ' CLLong t l r = t K.+ K.P 1

instance (Inhabited CLLong l r, PlusCtx CLLong l r) => Plus CLLong l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CLLong l r, Zero CLLong l r, PlusInvCtx CLLong l r)
  => PlusInv CLLong l r where
  type PlusInvCtx CLLong l r = l == K.Negate r
  plusinv = UnsafeI . negate . unwrap

instance (Inhabited CLLong l r, MultCtx CLLong l r) => Mult CLLong l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CLLong l r, MinusCtx CLLong l r) => Minus CLLong l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CLLong l r, ZeroCtx CLLong l r) => Zero CLLong l r where
  type ZeroCtx CLLong l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CLLong l r, OneCtx CLLong l r) => One CLLong l r where
  type OneCtx CLLong l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

