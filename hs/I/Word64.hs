-- File generated from Word64.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Word64 () where

import Control.Monad
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Data.Type.Ord
import Data.Word
import GHC.TypeLits qualified as Lits
import GHC.TypeNats (KnownNat)
import Prelude hiding (min, max, div)

import I.Internal

--------------------------------------------------------------------------------

type instance MinT Word64 = 0
type instance MaxT Word64 = 18446744073709551615
type instance MinL Word64 = MinT Word64
type instance MaxR Word64 = MaxT Word64

instance forall l r.
  ( IntervalCtx Word64 l r
  ) => Interval Word64 l r where
  type IntervalCtx Word64 l r =
    ( KnownNat l
    , KnownNat r
    , MinT Word64 <= l
    , l <= r
    , r <= MaxT Word64 )
  type MinBoundI Word64 l r = l
  type MaxBoundI Word64 l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNat @l @x
    Dict <- leNat @x @r
    pure (UnsafeI x)

instance
  ( Interval Word64 l r, InhabitedCtx Word64 l r
  ) => Inhabited Word64 l r where
  type InhabitedCtx Word64 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Word64 l r, KnownCtx Word64 t l r
  ) => Known Word64 t l r where
  type KnownCtx Word64 t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited Word64 l r) => With Word64 l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNat @l @t
    Dict <- leNat @t @r
    pure (g pt)

instance
  ( Inhabited Word64 l r, PredCtx Word64 l r
  ) => Pred Word64 l r where
  type PredCtx Word64 l r = (l /~ r)
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Word64 l r, SuccCtx Word64 l r
  ) => Succ Word64 l r where
  type SuccCtx Word64 l r = (l /~ r)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known Word64 t l r, Pred Word64 l r, KnownPredCtx Word64 t l r
  ) => KnownPred Word64 t l r where
  type KnownPredCtx Word64 t l r = t /~ l
  type Pred' Word64 t l r = t Lits.- 1
instance
  ( Known Word64 t l r, Succ Word64 l r, KnownSuccCtx Word64 t l r
  ) => KnownSucc Word64 t l r where
  type KnownSuccCtx Word64 t l r = t /~ r
  type Succ' Word64 t l r = t Lits.+ 1

instance (Inhabited Word64 l r, PlusCtx Word64 l r) => Plus Word64 l r
instance (Plus Word64 l r, Zero Word64 l r, PlusInvCtx Word64 l r)
  => PlusInv Word64 l r
instance (Inhabited Word64 l r, MultCtx Word64 l r) => Mult Word64 l r
instance (Inhabited Word64 l r, MinusCtx Word64 l r) => Minus Word64 l r
instance (Inhabited Word64 l r, ZeroCtx Word64 l r) => Zero Word64 l r where
  type ZeroCtx Word64 l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0
instance (Inhabited Word64 l r, OneCtx Word64 l r) => One Word64 l r where
  type OneCtx Word64 l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
