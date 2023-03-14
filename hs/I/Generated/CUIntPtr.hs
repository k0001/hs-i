-- File generated from CUIntPtr.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUIntPtr () where

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


type instance MinL CUIntPtr = MinT CUIntPtr
type instance MaxR CUIntPtr = MaxT CUIntPtr

instance forall l r.
  ( IntervalCtx CUIntPtr l r
  ) => Interval CUIntPtr l r where
  type IntervalCtx CUIntPtr l r =
    ( KnownNat l
    , KnownNat r
    , MinT CUIntPtr <= l
    , l <= r
    , r <= MaxT CUIntPtr )
  type MinBoundI CUIntPtr l r = l
  type MaxBoundI CUIntPtr l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CUIntPtr l r, InhabitedCtx CUIntPtr l r
  ) => Inhabited CUIntPtr l r where
  type InhabitedCtx CUIntPtr l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CUIntPtr l r, KnownCtx CUIntPtr t l r
  ) => Known CUIntPtr t l r where
  type KnownCtx CUIntPtr t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited CUIntPtr l r) => With CUIntPtr l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUIntPtr l r, PredCtx CUIntPtr l r
  ) => Pred CUIntPtr l r where
  type PredCtx CUIntPtr l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CUIntPtr l r, SuccCtx CUIntPtr l r
  ) => Succ CUIntPtr l r where
  type SuccCtx CUIntPtr l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CUIntPtr t l r, Pred CUIntPtr l r, KnownPredCtx CUIntPtr t l r
  ) => KnownPred CUIntPtr t l r where
  type KnownPredCtx CUIntPtr t l r = t /= l
  type Pred' CUIntPtr t l r = t Lits.- 1
instance
  ( Known CUIntPtr t l r, Succ CUIntPtr l r, KnownSuccCtx CUIntPtr t l r
  ) => KnownSucc CUIntPtr t l r where
  type KnownSuccCtx CUIntPtr t l r = t /= r
  type Succ' CUIntPtr t l r = t Lits.+ 1

instance (Inhabited CUIntPtr l r, PlusCtx CUIntPtr l r) => Plus CUIntPtr l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited CUIntPtr l r, MultCtx CUIntPtr l r) => Mult CUIntPtr l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CUIntPtr l r, MinusCtx CUIntPtr l r) => Minus CUIntPtr l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CUIntPtr l r, ZeroCtx CUIntPtr l r) => Zero CUIntPtr l r where
  type ZeroCtx CUIntPtr l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CUIntPtr l r, OneCtx CUIntPtr l r) => One CUIntPtr l r where
  type OneCtx CUIntPtr l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
