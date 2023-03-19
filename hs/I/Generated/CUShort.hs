-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUShort () where

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


type instance MinL CUShort = MinT CUShort
type instance MaxR CUShort = MaxT CUShort

instance forall l r.
  ( IntervalCtx CUShort l r
  ) => Interval CUShort l r where
  type IntervalCtx CUShort l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUShort <= l
    , l <= r
    , r <= MaxT CUShort )
  type MinI CUShort l r = l
  type MaxI CUShort l r = r

instance
  ( Interval CUShort l r, InhabitedCtx CUShort l r
  ) => Inhabited CUShort l r where
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

instance (Inhabited CUShort l r) => Clamp CUShort l r

instance (Inhabited CUShort ld rd, Inhabited CUShort lu ru, lu <= ld, rd <= ru)
  => Up CUShort ld rd lu ru

instance forall t l r.
  ( Inhabited CUShort l r, KnownCtx CUShort t l r
  ) => Known CUShort t l r where
  type KnownCtx CUShort t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited CUShort l r) => With CUShort l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUShort l r, l /= r
  ) => Discrete CUShort l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CUShort 0 r) => Zero CUShort 0 r where
  zero = UnsafeI 0

instance (Inhabited CUShort l r, l <= 1, 1 <= r) => One CUShort l r where
  one = UnsafeI 1
