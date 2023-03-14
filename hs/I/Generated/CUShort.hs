-- File generated from CUShort.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUShort () where

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


type instance MinL CUShort = MinT CUShort
type instance MaxR CUShort = MaxT CUShort

instance forall l r.
  ( IntervalCtx CUShort l r
  ) => Interval CUShort l r where
  type IntervalCtx CUShort l r =
    ( KnownNat l
    , KnownNat r
    , MinT CUShort <= l
    , l <= r
    , r <= MaxT CUShort )
  type MinBoundI CUShort l r = l
  type MaxBoundI CUShort l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CUShort l r, InhabitedCtx CUShort l r
  ) => Inhabited CUShort l r where
  type InhabitedCtx CUShort l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CUShort l r, KnownCtx CUShort t l r
  ) => Known CUShort t l r where
  type KnownCtx CUShort t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited CUShort l r) => With CUShort l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUShort l r, PredCtx CUShort l r
  ) => Pred CUShort l r where
  type PredCtx CUShort l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CUShort l r, SuccCtx CUShort l r
  ) => Succ CUShort l r where
  type SuccCtx CUShort l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CUShort t l r, Pred CUShort l r, KnownPredCtx CUShort t l r
  ) => KnownPred CUShort t l r where
  type KnownPredCtx CUShort t l r = t /= l
  type Pred' CUShort t l r = t Lits.- 1
instance
  ( Known CUShort t l r, Succ CUShort l r, KnownSuccCtx CUShort t l r
  ) => KnownSucc CUShort t l r where
  type KnownSuccCtx CUShort t l r = t /= r
  type Succ' CUShort t l r = t Lits.+ 1

instance (Inhabited CUShort l r, PlusCtx CUShort l r) => Plus CUShort l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited CUShort l r, MultCtx CUShort l r) => Mult CUShort l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CUShort l r, MinusCtx CUShort l r) => Minus CUShort l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CUShort l r, ZeroCtx CUShort l r) => Zero CUShort l r where
  type ZeroCtx CUShort l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CUShort l r, OneCtx CUShort l r) => One CUShort l r where
  type OneCtx CUShort l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
