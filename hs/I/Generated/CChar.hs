-- File generated from CChar.hs by geninstances.sh. Do not modify.
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
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CChar l r, InhabitedCtx CChar l r
  ) => Inhabited CChar l r where
  type InhabitedCtx CChar l r = ()
  inhabitant = min

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

instance
  ( Inhabited CChar l r, PredCtx CChar l r
  ) => Pred CChar l r where
  type PredCtx CChar l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CChar l r, SuccCtx CChar l r
  ) => Succ CChar l r where
  type SuccCtx CChar l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CChar l r, PlusCtx CChar l r) => Plus CChar l r where
  type PlusCtx CChar l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CChar l r, Zero CChar l r, MayNegateCtx CChar l r)
  => MayNegate CChar l r where
  type MayNegateCtx CChar l r = l < K.P 0
  negate' x = from =<< toIntegralSized (P.negate (toInteger (unwrap x)))

instance (MayNegate CChar l r, NegateCtx CChar l r)
  => Negate CChar l r where
  type NegateCtx CChar l r = l == K.Negate r
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CChar l r, MultCtx CChar l r) => Mult CChar l r where
  type MultCtx CChar l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CChar l r, MinusCtx CChar l r) => Minus CChar l r where
  type MinusCtx CChar l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CChar l r, ZeroCtx CChar l r) => Zero CChar l r where
  type ZeroCtx CChar l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CChar l r, OneCtx CChar l r) => One CChar l r where
  type OneCtx CChar l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

