-- File generated from CUChar.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUChar () where

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


type instance MinL CUChar = MinT CUChar
type instance MaxR CUChar = MaxT CUChar

instance forall l r.
  ( IntervalCtx CUChar l r
  ) => Interval CUChar l r where
  type IntervalCtx CUChar l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUChar <= l
    , l <= r
    , r <= MaxT CUChar )
  type MinI CUChar l r = l
  type MaxI CUChar l r = r
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CUChar l r, InhabitedCtx CUChar l r
  ) => Inhabited CUChar l r where
  type InhabitedCtx CUChar l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CUChar l r, KnownCtx CUChar t l r
  ) => Known CUChar t l r where
  type KnownCtx CUChar t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited CUChar l r) => With CUChar l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUChar l r, PredCtx CUChar l r
  ) => Pred CUChar l r where
  type PredCtx CUChar l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CUChar l r, SuccCtx CUChar l r
  ) => Succ CUChar l r where
  type SuccCtx CUChar l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CUChar l r, PlusCtx CUChar l r) => Plus CUChar l r where
  type PlusCtx CUChar l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited CUChar l r, MultCtx CUChar l r) => Mult CUChar l r where
  type MultCtx CUChar l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CUChar l r, MinusCtx CUChar l r) => Minus CUChar l r where
  type MinusCtx CUChar l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CUChar 0 r, ZeroCtx CUChar 0 r) => Zero CUChar 0 r where
  type ZeroCtx CUChar 0 r = ()
  zero = UnsafeI 0

instance (Inhabited CUChar l r, OneCtx CUChar l r) => One CUChar l r where
  type OneCtx CUChar l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
