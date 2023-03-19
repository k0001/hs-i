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

instance
  ( Interval Word8 l r, InhabitedCtx Word8 l r
  ) => Inhabited Word8 l r where
  inhabitant = min
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)
  a `plus'` b = do let x = unwrap a + unwrap b
                   guard (x >= unwrap a)
                   from x
  a `mult'` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                          toInteger (unwrap b))
  a `minus'` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                           toInteger (unwrap b))
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance (Inhabited Word8 l r) => Clamp Word8 l r

instance (Inhabited Word8 ld rd, Inhabited Word8 lu ru, lu <= ld, rd <= ru)
  => Up Word8 ld rd lu ru

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
  ( Inhabited Word8 l r, l /= r
  ) => Discrete Word8 l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Word8 0 r) => Zero Word8 0 r where
  zero = UnsafeI 0

instance (Inhabited Word8 l r, l <= 1, 1 <= r) => One Word8 l r where
  one = UnsafeI 1
