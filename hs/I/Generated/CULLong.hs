-- File generated from CULLong.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CULLong () where

import Control.Monad
import Data.Bits
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Data.Word
import Data.Type.Ord
import Foreign.C.Types
import GHC.TypeLits qualified as Lits
import GHC.TypeNats (KnownNat)
import KindInteger (type (/=))
import Prelude hiding (min, max, div)

import I.Internal

--------------------------------------------------------------------------------

-- | This is so that GHC doesn't complain about the unused modules,
-- which we import here so that `genmodules.sh` doesn't have to add it
-- to the generated modules.
_ignore :: (CSize, Word)
_ignore = (0, 0)

--------------------------------------------------------------------------------


type instance MinL CULLong = MinT CULLong
type instance MaxR CULLong = MaxT CULLong

instance forall l r.
  ( IntervalCtx CULLong l r
  ) => Interval CULLong l r where
  type IntervalCtx CULLong l r =
    ( KnownNat l
    , KnownNat r
    , MinT CULLong <= l
    , l <= r
    , r <= MaxT CULLong )
  type MinBoundI CULLong l r = l
  type MaxBoundI CULLong l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CULLong l r, InhabitedCtx CULLong l r
  ) => Inhabited CULLong l r where
  type InhabitedCtx CULLong l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CULLong l r, KnownCtx CULLong t l r
  ) => Known CULLong t l r where
  type KnownCtx CULLong t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited CULLong l r) => With CULLong l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CULLong l r, PredCtx CULLong l r
  ) => Pred CULLong l r where
  type PredCtx CULLong l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CULLong l r, SuccCtx CULLong l r
  ) => Succ CULLong l r where
  type SuccCtx CULLong l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CULLong t l r, Pred CULLong l r, KnownPredCtx CULLong t l r
  ) => KnownPred CULLong t l r where
  type KnownPredCtx CULLong t l r = t /= l
  type Pred' CULLong t l r = t Lits.- 1
instance
  ( Known CULLong t l r, Succ CULLong l r, KnownSuccCtx CULLong t l r
  ) => KnownSucc CULLong t l r where
  type KnownSuccCtx CULLong t l r = t /= r
  type Succ' CULLong t l r = t Lits.+ 1

instance (Inhabited CULLong l r, PlusCtx CULLong l r) => Plus CULLong l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited CULLong l r, MultCtx CULLong l r) => Mult CULLong l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CULLong l r, MinusCtx CULLong l r) => Minus CULLong l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CULLong l r, ZeroCtx CULLong l r) => Zero CULLong l r where
  type ZeroCtx CULLong l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CULLong l r, OneCtx CULLong l r) => One CULLong l r where
  type OneCtx CULLong l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
