-- File generated from Word64.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Word64 () where

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


type instance MinL Word64 = MinT Word64
type instance MaxR Word64 = MaxT Word64

instance forall l r.
  ( IntervalCtx Word64 l r
  ) => Interval Word64 l r where
  type IntervalCtx Word64 l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT Word64 <= l
    , l <= r
    , r <= MaxT Word64 )
  type MinI Word64 l r = l
  type MaxI Word64 l r = r
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval Word64 l r, InhabitedCtx Word64 l r
  ) => Inhabited Word64 l r where
  type InhabitedCtx Word64 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Word64 l r, KnownCtx Word64 t l r
  ) => Known Word64 t l r where
  type KnownCtx Word64 t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited Word64 l r) => With Word64 l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited Word64 l r, PredCtx Word64 l r
  ) => Pred Word64 l r where
  type PredCtx Word64 l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Word64 l r, SuccCtx Word64 l r
  ) => Succ Word64 l r where
  type SuccCtx Word64 l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Word64 l r, PlusCtx Word64 l r) => Plus Word64 l r where
  type PlusCtx Word64 l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited Word64 l r, MultCtx Word64 l r) => Mult Word64 l r where
  type MultCtx Word64 l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited Word64 l r, MinusCtx Word64 l r) => Minus Word64 l r where
  type MinusCtx Word64 l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited Word64 0 r, ZeroCtx Word64 0 r) => Zero Word64 0 r where
  type ZeroCtx Word64 0 r = ()
  zero = UnsafeI 0

instance (Inhabited Word64 l r, OneCtx Word64 l r) => One Word64 l r where
  type OneCtx Word64 l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
