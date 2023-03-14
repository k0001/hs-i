-- File generated from CPtrdiff.hs by geninstances.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CPtrdiff () where

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

type instance MinL CPtrdiff = MinT CPtrdiff
type instance MaxR CPtrdiff = MaxT CPtrdiff

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CPtrdiff l r
  ) => Interval CPtrdiff l r where
  type IntervalCtx CPtrdiff l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CPtrdiff <= l
    , l <= r
    , r <= MaxT CPtrdiff )
  type MinI CPtrdiff l r = l
  type MaxI CPtrdiff l r = r
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)

instance
  ( Interval CPtrdiff l r, InhabitedCtx CPtrdiff l r
  ) => Inhabited CPtrdiff l r where
  type InhabitedCtx CPtrdiff l r = ()
  inhabitant = min

instance forall t l r.
  ( Inhabited CPtrdiff l r, KnownCtx CPtrdiff t l r
  ) => Known CPtrdiff t l r where
  type KnownCtx CPtrdiff t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CPtrdiff l r) => With CPtrdiff l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance
  ( Inhabited CPtrdiff l r, PredCtx CPtrdiff l r
  ) => Pred CPtrdiff l r where
  type PredCtx CPtrdiff l r = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited CPtrdiff l r, SuccCtx CPtrdiff l r
  ) => Succ CPtrdiff l r where
  type SuccCtx CPtrdiff l r = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CPtrdiff l r, PlusCtx CPtrdiff l r) => Plus CPtrdiff l r where
  type PlusCtx CPtrdiff l r = ()
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

instance (Plus CPtrdiff l r, Zero CPtrdiff l r, MayNegateCtx CPtrdiff l r)
  => MayNegate CPtrdiff l r where
  type MayNegateCtx CPtrdiff l r = l < K.P 0
  negate' x = from =<< toIntegralSized (P.negate (toInteger (unwrap x)))

instance (MayNegate CPtrdiff l r, NegateCtx CPtrdiff l r)
  => Negate CPtrdiff l r where
  type NegateCtx CPtrdiff l r = l == K.Negate r
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CPtrdiff l r, MultCtx CPtrdiff l r) => Mult CPtrdiff l r where
  type MultCtx CPtrdiff l r = ()
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

instance (Inhabited CPtrdiff l r, MinusCtx CPtrdiff l r) => Minus CPtrdiff l r where
  type MinusCtx CPtrdiff l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited CPtrdiff l r, ZeroCtx CPtrdiff l r) => Zero CPtrdiff l r where
  type ZeroCtx CPtrdiff l r = (l <= K.P 0, K.P 0 <= r)
  zero = UnsafeI 0

instance (Inhabited CPtrdiff l r, OneCtx CPtrdiff l r) => One CPtrdiff l r where
  type OneCtx CPtrdiff l r = (l <= K.P 1, K.P 1 <= r)
  one = UnsafeI 1

