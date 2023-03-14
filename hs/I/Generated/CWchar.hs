-- File generated from CWchar.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CWchar () where

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

type instance MinL CWchar = MinT CWchar
type instance MaxR CWchar = MaxT CWchar

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CWchar l r
  ) => Interval CWchar l r where
  type IntervalCtx CWchar l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CWchar <= l
    , l <= r
    , r <= MaxT CWchar )
  type MinI CWchar l r = l
  type MaxI CWchar l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CWchar l r, InhabitedCtx CWchar l r
  ) => Inhabited CWchar l r where
  type InhabitedCtx CWchar l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CWchar l r, KnownCtx CWchar t l r
  ) => Known CWchar t l r where
  type KnownCtx CWchar t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CWchar l r) => With CWchar l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited CWchar l r, PredCtx CWchar l r
  ) => Pred CWchar l r where
  type PredCtx CWchar l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CWchar l r, SuccCtx CWchar l r
  ) => Succ CWchar l r where
  type SuccCtx CWchar l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CWchar l r, PlusCtx CWchar l r) => Plus CWchar l r where
  type PlusCtx CWchar l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CWchar l r, Zero CWchar l r, MayNegateCtx CWchar l r)
  => MayNegate CWchar l r where
  type MayNegateCtx CWchar l r = l < K.P 0
  negate' x = from =<< toIntegralSized (P.negate (toInteger (unwrap x)))

instance (MayNegate CWchar l r, NegateCtx CWchar l r)
  => Negate CWchar l r where
  type NegateCtx CWchar l r = l == K.Negate r
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CWchar l r, MultCtx CWchar l r) => Mult CWchar l r where
  type MultCtx CWchar l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CWchar l r, MinusCtx CWchar l r) => Minus CWchar l r where
  type MinusCtx CWchar l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CWchar l r, ZeroCtx CWchar l r) => Zero CWchar l r where
  type ZeroCtx CWchar l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CWchar l r, OneCtx CWchar l r) => One CWchar l r where
  type OneCtx CWchar l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

