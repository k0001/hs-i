-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CChar () where

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

type instance MinL CChar = MinT CChar
type instance MaxR CChar = MaxT CChar

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CChar l r
  ) => Interval CChar l r where
  type IntervalCtx CChar l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CChar <= l
    , l <= r
    , r <= MaxT CChar )
  type MinI CChar l r = l
  type MaxI CChar l r = r

instance
  ( Interval CChar l r, InhabitedCtx CChar l r
  ) => Inhabited CChar l r where
  type InhabitedCtx CChar l r = ()
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
  ( Inhabited CChar l r, KnownCtx CChar t l r
  ) => Known CChar t l r where
  type KnownCtx CChar t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CChar l r) => With CChar l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited CChar l r, l /= r) => Discrete CChar l r where
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero CChar l r, l == K.Negate r) => Negate CChar l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CChar l r, l <= K.P 0, K.P 0 <= r) => Zero CChar l r where
  zero = UnsafeI 0

instance (Inhabited CChar l r, l <= K.P 1, K.P 1 <= r) => One CChar l r where
  one = UnsafeI 1

