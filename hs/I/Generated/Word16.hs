-- File generated from Word16.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Word16 () where

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


type instance MinL Word16 = MinT Word16
type instance MaxR Word16 = MaxT Word16

instance forall l r.
  ( IntervalCtx Word16 l r
  ) => Interval Word16 l r where
  type IntervalCtx Word16 l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT Word16 <= l
    , l <= r
    , r <= MaxT Word16 )
  type MinI Word16 l r = l
  type MaxI Word16 l r = r
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval Word16 l r, InhabitedCtx Word16 l r
  ) => Inhabited Word16 l r where
  type InhabitedCtx Word16 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Word16 l r, KnownCtx Word16 t l r
  ) => Known Word16 t l r where
  type KnownCtx Word16 t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited Word16 l r) => With Word16 l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited Word16 l r, PredCtx Word16 l r
  ) => Pred Word16 l r where
  type PredCtx Word16 l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Word16 l r, SuccCtx Word16 l r
  ) => Succ Word16 l r where
  type SuccCtx Word16 l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Word16 l r, PlusCtx Word16 l r) => Plus Word16 l r where
  type PlusCtx Word16 l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited Word16 l r, MultCtx Word16 l r) => Mult Word16 l r where
  type MultCtx Word16 l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited Word16 l r, MinusCtx Word16 l r) => Minus Word16 l r where
  type MinusCtx Word16 l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited Word16 0 r, ZeroCtx Word16 0 r) => Zero Word16 0 r where
  type ZeroCtx Word16 0 r = ()
  zero = UnsafeI 0

instance (Inhabited Word16 l r, OneCtx Word16 l r) => One Word16 l r where
  type OneCtx Word16 l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
