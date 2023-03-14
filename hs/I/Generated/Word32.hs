-- File generated from Word32.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Word32 () where

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


type instance MinL Word32 = MinT Word32
type instance MaxR Word32 = MaxT Word32

instance forall l r.
  ( IntervalCtx Word32 l r
  ) => Interval Word32 l r where
  type IntervalCtx Word32 l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT Word32 <= l
    , l <= r
    , r <= MaxT Word32 )
  type MinBoundI Word32 l r = l
  type MaxBoundI Word32 l r = r
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval Word32 l r, InhabitedCtx Word32 l r
  ) => Inhabited Word32 l r where
  type InhabitedCtx Word32 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Word32 l r, KnownCtx Word32 t l r
  ) => Known Word32 t l r where
  type KnownCtx Word32 t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited Word32 l r) => With Word32 l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited Word32 l r, PredCtx Word32 l r
  ) => Pred Word32 l r where
  type PredCtx Word32 l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Word32 l r, SuccCtx Word32 l r
  ) => Succ Word32 l r where
  type SuccCtx Word32 l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Word32 l r, PlusCtx Word32 l r) => Plus Word32 l r where
  type PlusCtx Word32 l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited Word32 l r, MultCtx Word32 l r) => Mult Word32 l r where
  type MultCtx Word32 l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited Word32 l r, MinusCtx Word32 l r) => Minus Word32 l r where
  type MinusCtx Word32 l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited Word32 l r, ZeroCtx Word32 l r) => Zero Word32 l r where
  type ZeroCtx Word32 l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0

instance (Inhabited Word32 l r, OneCtx Word32 l r) => One Word32 l r where
  type OneCtx Word32 l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
