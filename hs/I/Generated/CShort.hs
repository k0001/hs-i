-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CShort () where

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

type instance MinL CShort = MinT CShort
type instance MaxR CShort = MaxT CShort

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CShort l r
  ) => Interval CShort l r where
  type IntervalCtx CShort l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CShort <= l
    , l <= r
    , r <= MaxT CShort )
  type MinI CShort l r = l
  type MaxI CShort l r = r

instance
  ( Interval CShort l r, InhabitedCtx CShort l r
  ) => Inhabited CShort l r where
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)
  negate' x = do guard (unwrap x /= minBound)
                 from (P.negate (unwrap x))
  a `plus'` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                          toInteger (unwrap b))
  a `mult'` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                          toInteger (unwrap b))
  a `minus'` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                           toInteger (unwrap b))
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance (Inhabited CShort l r) => Clamp CShort l r

instance (Inhabited CShort ld rd, Inhabited CShort lu ru, lu <= ld, rd <= ru)
  => Up CShort ld rd lu ru

instance forall t l r.
  ( Inhabited CShort l r, KnownCtx CShort t l r
  ) => Known CShort t l r where
  type KnownCtx CShort t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CShort l r) => With CShort l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited CShort l r, l /= r) => Discrete CShort l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero CShort l r, l == K.Negate r) => Negate CShort l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CShort l r, l <= K.P 0, K.P 0 <= r) => Zero CShort l r where
  zero = UnsafeI 0

instance (Inhabited CShort l r, l <= K.P 1, K.P 1 <= r) => One CShort l r where
  one = UnsafeI 1

