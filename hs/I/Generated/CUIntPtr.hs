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


type instance MinL CUIntPtr = MinT CUIntPtr
type instance MaxR CUIntPtr = MaxT CUIntPtr

instance forall l r.
  ( IntervalCtx CUIntPtr l r
  ) => Interval CUIntPtr l r where
  type IntervalCtx CUIntPtr l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUIntPtr <= l
    , l <= r
    , r <= MaxT CUIntPtr )
  type MinI CUIntPtr l r = l
  type MaxI CUIntPtr l r = r

instance
  ( Interval CUIntPtr l r, InhabitedCtx CUIntPtr l r
  ) => Inhabited CUIntPtr l r where
  type InhabitedCtx CUIntPtr l r = ()
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
  ( Inhabited CUIntPtr l r, KnownCtx CUIntPtr t l r
  ) => Known CUIntPtr t l r where
  type KnownCtx CUIntPtr t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited CUIntPtr l r) => With CUIntPtr l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUIntPtr l r, l /= r
  ) => Discrete CUIntPtr l r where
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CUIntPtr 0 r) => Zero CUIntPtr 0 r where
  zero = UnsafeI 0

instance (Inhabited CUIntPtr l r, l <= 1, 1 <= r) => One CUIntPtr l r where
  one = UnsafeI 1
