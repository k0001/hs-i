-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUInt () where

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


type instance MinL CUInt = MinT CUInt
type instance MaxR CUInt = MaxT CUInt

instance forall l r.
  ( IntervalCtx CUInt l r
  ) => Interval CUInt l r where
  type IntervalCtx CUInt l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUInt <= l
    , l <= r
    , r <= MaxT CUInt )
  type MinI CUInt l r = l
  type MaxI CUInt l r = r

instance
  ( Interval CUInt l r, InhabitedCtx CUInt l r
  ) => Inhabited CUInt l r where
  inhabitant = min
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)
  a `plus'` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                          toInteger (unwrap b))
  a `mult'` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                          toInteger (unwrap b))
  a `minus'` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                           toInteger (unwrap b))
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance (Inhabited CUInt l r) => Clamp CUInt l r

instance (Inhabited CUInt ld rd, Inhabited CUInt lu ru, lu <= ld, rd <= ru)
  => Up CUInt ld rd lu ru

instance forall t l r.
  ( Inhabited CUInt l r, KnownCtx CUInt t l r
  ) => Known CUInt t l r where
  type KnownCtx CUInt t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited CUInt l r) => With CUInt l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUInt l r, l /= r
  ) => Discrete CUInt l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CUInt 0 r) => Zero CUInt 0 r where
  zero = UnsafeI 0

instance (Inhabited CUInt l r, l <= 1, 1 <= r) => One CUInt l r where
  one = UnsafeI 1
