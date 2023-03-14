-- File generated from CULong.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CULong () where

import Control.Monad
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


type instance MinL CULong = MinT CULong
type instance MaxR CULong = MaxT CULong

instance forall l r.
  ( IntervalCtx CULong l r
  ) => Interval CULong l r where
  type IntervalCtx CULong l r =
    ( KnownNat l
    , KnownNat r
    , MinT CULong <= l
    , l <= r
    , r <= MaxT CULong )
  type MinBoundI CULong l r = l
  type MaxBoundI CULong l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval CULong l r, InhabitedCtx CULong l r
  ) => Inhabited CULong l r where
  type InhabitedCtx CULong l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CULong l r, KnownCtx CULong t l r
  ) => Known CULong t l r where
  type KnownCtx CULong t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited CULong l r) => With CULong l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CULong l r, PredCtx CULong l r
  ) => Pred CULong l r where
  type PredCtx CULong l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CULong l r, SuccCtx CULong l r
  ) => Succ CULong l r where
  type SuccCtx CULong l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known CULong t l r, Pred CULong l r, KnownPredCtx CULong t l r
  ) => KnownPred CULong t l r where
  type KnownPredCtx CULong t l r = t /= l
  type Pred' CULong t l r = t Lits.- 1
instance
  ( Known CULong t l r, Succ CULong l r, KnownSuccCtx CULong t l r
  ) => KnownSucc CULong t l r where
  type KnownSuccCtx CULong t l r = t /= r
  type Succ' CULong t l r = t Lits.+ 1

instance (Inhabited CULong l r, PlusCtx CULong l r) => Plus CULong l r
instance (Inhabited CULong l r, MultCtx CULong l r) => Mult CULong l r
instance (Inhabited CULong l r, MinusCtx CULong l r) => Minus CULong l r
instance (Inhabited CULong l r, ZeroCtx CULong l r) => Zero CULong l r where
  type ZeroCtx CULong l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0
instance (Inhabited CULong l r, OneCtx CULong l r) => One CULong l r where
  type OneCtx CULong l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
