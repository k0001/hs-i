-- File generated from CUIntMax.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUIntMax () where

import Control.Monad
import Data.Bits
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Data.Word
import Data.Type.Ord
import Foreign.C.Types
import GHC.TypeLits qualified as L
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


type instance MinL CUIntMax = MinT CUIntMax
type instance MaxR CUIntMax = MaxT CUIntMax

instance forall l r.
  ( IntervalCtx CUIntMax l r
  ) => Interval CUIntMax l r where
  type IntervalCtx CUIntMax l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUIntMax <= l
    , l <= r
    , r <= MaxT CUIntMax )
  type MinBoundI CUIntMax l r = l
  type MaxBoundI CUIntMax l r = r
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CUIntMax l r, InhabitedCtx CUIntMax l r
  ) => Inhabited CUIntMax l r where
  type InhabitedCtx CUIntMax l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CUIntMax l r, KnownCtx CUIntMax t l r
  ) => Known CUIntMax t l r where
  type KnownCtx CUIntMax t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited CUIntMax l r) => With CUIntMax l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUIntMax l r, PredCtx CUIntMax l r
  ) => Pred CUIntMax l r where
  type PredCtx CUIntMax l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CUIntMax l r, SuccCtx CUIntMax l r
  ) => Succ CUIntMax l r where
  type SuccCtx CUIntMax l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CUIntMax l r, PlusCtx CUIntMax l r) => Plus CUIntMax l r where
  type PlusCtx CUIntMax l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited CUIntMax l r, MultCtx CUIntMax l r) => Mult CUIntMax l r where
  type MultCtx CUIntMax l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CUIntMax l r, MinusCtx CUIntMax l r) => Minus CUIntMax l r where
  type MinusCtx CUIntMax l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CUIntMax l r, ZeroCtx CUIntMax l r) => Zero CUIntMax l r where
  type ZeroCtx CUIntMax l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CUIntMax l r, OneCtx CUIntMax l r) => One CUIntMax l r where
  type OneCtx CUIntMax l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
