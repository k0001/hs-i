-- File generated from CUInt.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUInt () where

import Control.Monad
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


type instance MinL CUInt = MinT CUInt
type instance MaxR CUInt = MaxT CUInt

instance forall l r.
  ( IntervalCtx CUInt l r
  ) => Interval CUInt l r where
  type IntervalCtx CUInt l r =
    ( KnownNat l
    , KnownNat r
    , MinT CUInt <= l
    , l <= r
    , r <= MaxT CUInt )
  type MinBoundI CUInt l r = l
  type MaxBoundI CUInt l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CUInt l r, InhabitedCtx CUInt l r
  ) => Inhabited CUInt l r where
  type InhabitedCtx CUInt l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CUInt l r, KnownCtx CUInt t l r
  ) => Known CUInt t l r where
  type KnownCtx CUInt t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited CUInt l r) => With CUInt l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUInt l r, PredCtx CUInt l r
  ) => Pred CUInt l r where
  type PredCtx CUInt l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CUInt l r, SuccCtx CUInt l r
  ) => Succ CUInt l r where
  type SuccCtx CUInt l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CUInt t l r, Pred CUInt l r, KnownPredCtx CUInt t l r
  ) => KnownPred CUInt t l r where
  type KnownPredCtx CUInt t l r = t /= l
  type Pred' CUInt t l r = t Lits.- 1
instance
  ( Known CUInt t l r, Succ CUInt l r, KnownSuccCtx CUInt t l r
  ) => KnownSucc CUInt t l r where
  type KnownSuccCtx CUInt t l r = t /= r
  type Succ' CUInt t l r = t Lits.+ 1

instance (Inhabited CUInt l r, PlusCtx CUInt l r) => Plus CUInt l r
instance (Inhabited CUInt l r, MultCtx CUInt l r) => Mult CUInt l r
instance (Inhabited CUInt l r, MinusCtx CUInt l r) => Minus CUInt l r
instance (Inhabited CUInt l r, ZeroCtx CUInt l r) => Zero CUInt l r where
  type ZeroCtx CUInt l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0
instance (Inhabited CUInt l r, OneCtx CUInt l r) => One CUInt l r where
  type OneCtx CUInt l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
