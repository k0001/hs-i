-- File generated from Word.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Word () where

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


type instance MinL Word = MinT Word
type instance MaxR Word = MaxT Word

instance forall l r.
  ( IntervalCtx Word l r
  ) => Interval Word l r where
  type IntervalCtx Word l r =
    ( KnownNat l
    , KnownNat r
    , MinT Word <= l
    , l <= r
    , r <= MaxT Word )
  type MinBoundI Word l r = l
  type MaxBoundI Word l r = r
  from x = do
    Lits.SomeNat (_ :: Proxy x) <- Lits.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval Word l r, InhabitedCtx Word l r
  ) => Inhabited Word l r where
  type InhabitedCtx Word l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Word l r, KnownCtx Word t l r
  ) => Known Word t l r where
  type KnownCtx Word t l r = (KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (Lits.natVal (Proxy @t)))

instance forall l r. (Inhabited Word l r) => With Word l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    Lits.SomeNat (pt :: Proxy t) <- Lits.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited Word l r, PredCtx Word l r
  ) => Pred Word l r where
  type PredCtx Word l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Word l r, SuccCtx Word l r
  ) => Succ Word l r where
  type SuccCtx Word l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Known Word t l r, Pred Word l r, KnownPredCtx Word t l r
  ) => KnownPred Word t l r where
  type KnownPredCtx Word t l r = t /= l
  type Pred' Word t l r = t Lits.- 1
instance
  ( Known Word t l r, Succ Word l r, KnownSuccCtx Word t l r
  ) => KnownSucc Word t l r where
  type KnownSuccCtx Word t l r = t /= r
  type Succ' Word t l r = t Lits.+ 1

instance (Inhabited Word l r, PlusCtx Word l r) => Plus Word l r
instance (Inhabited Word l r, MultCtx Word l r) => Mult Word l r
instance (Inhabited Word l r, MinusCtx Word l r) => Minus Word l r
instance (Inhabited Word l r, ZeroCtx Word l r) => Zero Word l r where
  type ZeroCtx Word l r = (l <= 0, 0 <= r)
  zero = UnsafeI 0
instance (Inhabited Word l r, OneCtx Word l r) => One Word l r where
  type OneCtx Word l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
