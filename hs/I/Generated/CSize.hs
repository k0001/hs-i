-- File generated from CSize.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CSize () where

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


type instance MinL CSize = MinT CSize
type instance MaxR CSize = MaxT CSize

instance forall l r.
  ( IntervalCtx CSize l r
  ) => Interval CSize l r where
  type IntervalCtx CSize l r =
    ( KnownNat l
    , KnownNat r
    , MinT CSize <= l
    , l <= r
    , r <= MaxT CSize )
  type MinBoundI CSize l r = l
  type MaxBoundI CSize l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CSize l r, InhabitedCtx CSize l r
  ) => Inhabited CSize l r where
  type InhabitedCtx CSize l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CSize l r, KnownCtx CSize t l r
  ) => Known CSize t l r where
  type KnownCtx CSize t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited CSize l r) => With CSize l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CSize l r, PredCtx CSize l r
  ) => Pred CSize l r where
  type PredCtx CSize l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CSize l r, SuccCtx CSize l r
  ) => Succ CSize l r where
  type SuccCtx CSize l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CSize t l r, Pred CSize l r, KnownPredCtx CSize t l r
  ) => KnownPred CSize t l r where
  type KnownPredCtx CSize t l r = t /= l
  type Pred' CSize t l r = t Lits.- 1
instance
  ( Known CSize t l r, Succ CSize l r, KnownSuccCtx CSize t l r
  ) => KnownSucc CSize t l r where
  type KnownSuccCtx CSize t l r = t /= r
  type Succ' CSize t l r = t Lits.+ 1

instance (Inhabited CSize l r, PlusCtx CSize l r) => Plus CSize l r where
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited CSize l r, MultCtx CSize l r) => Mult CSize l r where
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CSize l r, MinusCtx CSize l r) => Minus CSize l r where
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CSize l r, ZeroCtx CSize l r) => Zero CSize l r where
  type ZeroCtx CSize l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CSize l r, OneCtx CSize l r) => One CSize l r where
  type OneCtx CSize l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
