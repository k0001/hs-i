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

instance
  ( Interval Word64 l r, InhabitedCtx Word64 l r
  ) => Inhabited Word64 l r where
  type InhabitedCtx Word64 l r = ()
  inhabitant = min
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)
  negate' _ = Nothing
  recip' _ = Nothing
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

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
  ( Inhabited Word64 l r, l /= r
  ) => Discrete Word64 l r where
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Word64 0 r) => Zero Word64 0 r where
  zero = UnsafeI 0

instance (Inhabited Word64 l r, l <= 1, 1 <= r) => One Word64 l r where
  one = UnsafeI 1
