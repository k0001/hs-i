{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Word8 () where

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

type instance MinT Word8 = 0
type instance MaxT Word8 = 255
type instance MinL Word8 = MinT Word8
type instance MaxR Word8 = MaxT Word8

instance forall l r.
  ( IntervalCtx Word8 l r
  ) => Interval Word8 l r where
  type IntervalCtx Word8 l r =
    ( KnownNat l
    , KnownNat r
    , MinT Word8 <= l
    , l <= r
    , r <= MaxT Word8 )
  type MinBoundI Word8 l r = l
  type MaxBoundI Word8 l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNat @l @x
    Dict <- leNat @x @r
    pure (UnsafeI x)

instance
  ( Interval Word8 l r, InhabitedCtx Word8 l r
  ) => Inhabited Word8 l r where
  type InhabitedCtx Word8 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Word8 l r, KnownCtx Word8 t l r
  ) => Known Word8 t l r where
  type KnownCtx Word8 t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited Word8 l r) => With Word8 l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNat @l @t
    Dict <- leNat @t @r
    pure (g pt)

instance
  ( Inhabited Word8 l r, PredCtx Word8 l r
  ) => Pred Word8 l r where
  type PredCtx Word8 l r = (l /~ r)
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Word8 l r, SuccCtx Word8 l r
  ) => Succ Word8 l r where
  type SuccCtx Word8 l r = (l /~ r)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known Word8 t l r, Pred Word8 l r, KnownPredCtx Word8 t l r
  ) => KnownPred Word8 t l r where
  type KnownPredCtx Word8 t l r = t /~ l
  type Pred' Word8 t l r = t Lits.- 1
instance
  ( Known Word8 t l r, Succ Word8 l r, KnownSuccCtx Word8 t l r
  ) => KnownSucc Word8 t l r where
  type KnownSuccCtx Word8 t l r = t /~ r
  type Succ' Word8 t l r = t Lits.+ 1

instance (Inhabited Word8 l r, PlusCtx Word8 l r) => Plus Word8 l r
instance (Plus Word8 l r, Zero Word8 l r, PlusInvCtx Word8 l r)
  => PlusInv Word8 l r
instance (Inhabited Word8 l r, MultCtx Word8 l r) => Mult Word8 l r
instance (Inhabited Word8 l r, MinusCtx Word8 l r) => Minus Word8 l r
instance (Inhabited Word8 l r, ZeroCtx Word8 l r) => Zero Word8 l r where
  type ZeroCtx Word8 l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0
instance (Inhabited Word8 l r, OneCtx Word8 l r) => One Word8 l r where
  type OneCtx Word8 l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
