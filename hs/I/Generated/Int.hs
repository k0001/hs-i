-- File generated from Int.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.Int () where

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

type instance MinL Int = MinT Int
type instance MaxR Int = MaxT Int

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx Int l r
  ) => Interval Int l r where
  type IntervalCtx Int l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT Int <= l
    , l <= r
    , r <= MaxT Int )
  type MinI Int l r = l
  type MaxI Int l r = r

instance
  ( Interval Int l r, InhabitedCtx Int l r
  ) => Inhabited Int l r where
  type InhabitedCtx Int l r = ()
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
  ( Inhabited Int l r, KnownCtx Int t l r
  ) => Known Int t l r where
  type KnownCtx Int t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited Int l r) => With Int l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited Int l r, l /= r) => Discrete Int l r where
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero Int l r, l == K.Negate r) => Negate Int l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited Int l r, l <= K.P 0, K.P 0 <= r) => Zero Int l r where
  zero = UnsafeI 0

instance (Inhabited Int l r, l <= K.P 1, K.P 1 <= r) => One Int l r where
  one = UnsafeI 1

