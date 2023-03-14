-- File generated from Int16.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Int16 () where

import Control.Monad
import Data.Bits
import Data.Constraint
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Type.Ord
import Foreign.C.Types
import KindInteger (type (/=), type (==))
import KindInteger qualified as K
import Prelude hiding (min, max, div)
import Prelude qualified as P

import I.Internal

--------------------------------------------------------------------------------

-- | This is so that GHC doesn't complain about the unused modules,
-- which we import here so that `genmodules.sh` doesn't have to add it
-- to the generated modules.
_ignore :: (CSize, Int)
_ignore = (0, 0)

--------------------------------------------------------------------------------

type instance MinL Int16 = MinT Int16
type instance MaxR Int16 = MaxT Int16

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int16 l r
  ) => Interval Int16 l r where
  type IntervalCtx Int16 l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int16 <= l
    , l <= r
    , r <= MaxT Int16 )
  type MinI Int16 l r = l
  type MaxI Int16 l r = r

instance
  ( Interval Int16 l r, InhabitedCtx Int16 l r
  ) => Inhabited Int16 l r where
  type InhabitedCtx Int16 l r = ()
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)
  negate' x = from =<< toIntegralSized (P.negate (toInteger (unwrap x)))
  recip' _ = Nothing
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance forall t l r.
  ( Inhabited Int16 l r, KnownCtx Int16 t l r
  ) => Known Int16 t l r where
  type KnownCtx Int16 t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited Int16 l r) => With Int16 l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited Int16 l r, l /= r) => Discrete Int16 l r where
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero Int16 l r, l == K.Negate r) => Negate Int16 l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited Int16 l r, l <= K.P 0, K.P 0 <= r) => Zero Int16 l r where
  zero = UnsafeI 0

instance (Inhabited Int16 l r, l <= K.P 1, K.P 1 <= r) => One Int16 l r where
  one = UnsafeI 1

