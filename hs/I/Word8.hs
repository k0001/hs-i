{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Word8 () where

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


type instance MinL Word8 = MinT Word8
type instance MaxR Word8 = MaxT Word8

instance forall l r.
  ( IntervalCtx Word8 l r
  ) => Interval Word8 l r where
  type IntervalCtx Word8 l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT Word8 <= l
    , l <= r
    , r <= MaxT Word8 )
  type MinI Word8 l r = l
  type MaxI Word8 l r = r
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval Word8 l r, InhabitedCtx Word8 l r
  ) => Inhabited Word8 l r where
  type InhabitedCtx Word8 l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited Word8 l r, KnownCtx Word8 t l r
  ) => Known Word8 t l r where
  type KnownCtx Word8 t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited Word8 l r) => With Word8 l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited Word8 l r, PredCtx Word8 l r
  ) => Pred Word8 l r where
  type PredCtx Word8 l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Word8 l r, SuccCtx Word8 l r
  ) => Succ Word8 l r where
  type SuccCtx Word8 l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Word8 l r, PlusCtx Word8 l r) => Plus Word8 l r where
  type PlusCtx Word8 l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Inhabited Word8 l r, MultCtx Word8 l r) => Mult Word8 l r where
  type MultCtx Word8 l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited Word8 l r, MinusCtx Word8 l r) => Minus Word8 l r where
  type MinusCtx Word8 l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited Word8 0 r, ZeroCtx Word8 0 r) => Zero Word8 0 r where
  type ZeroCtx Word8 0 r = ()
  zero = UnsafeI 0

instance (Inhabited Word8 l r, OneCtx Word8 l r) => One Word8 l r where
  type OneCtx Word8 l r = (l <= 1, 1 <= r)
  one = UnsafeI 1
