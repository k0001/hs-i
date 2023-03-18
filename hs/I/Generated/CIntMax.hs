-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CIntMax () where

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

type instance MinL CIntMax = MinT CIntMax
type instance MaxR CIntMax = MaxT CIntMax

instance forall (l :: K.Integer) (r :: K.Integer).
  ( IntervalCtx CIntMax l r
  ) => Interval CIntMax l r where
  type IntervalCtx CIntMax l r =
    ( K.KnownInteger l
    , K.KnownInteger r
    , MinT CIntMax <= l
    , l <= r
    , r <= MaxT CIntMax )
  type MinI CIntMax l r = l
  type MaxI CIntMax l r = r

instance
  ( Interval CIntMax l r, InhabitedCtx CIntMax l r
  ) => Inhabited CIntMax l r where
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy t) <- K.someIntegerVal (toInteger x) = do
    Dict <- leInteger @l @t
    Dict <- leInteger @t @r
    pure (UnsafeI x)
  negate' x = from =<< toIntegralSized (P.negate (toInteger (unwrap x)))
  a `plus'` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                          toInteger (unwrap b))
  a `mult'` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                          toInteger (unwrap b))
  a `minus'` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                           toInteger (unwrap b))
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance (Inhabited CIntMax ld rd, Inhabited CIntMax lu ru, lu <= ld, rd <= ru)
  => Up CIntMax ld rd lu ru

instance forall t l r.
  ( Inhabited CIntMax l r, KnownCtx CIntMax t l r
  ) => Known CIntMax t l r where
  type KnownCtx CIntMax t l r = (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (fromInteger (K.integerVal (Proxy @t)))

instance forall l r. (Inhabited CIntMax l r) => With CIntMax l r where
  with x g
    | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (toInteger (unwrap x))
    = fromMaybe (error "I.with: impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance (Inhabited CIntMax l r, l /= r) => Discrete CIntMax l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Zero CIntMax l r, l == K.Negate r) => Negate CIntMax l r where
  negate = UnsafeI . P.negate . unwrap

instance (Inhabited CIntMax l r, l <= K.P 0, K.P 0 <= r) => Zero CIntMax l r where
  zero = UnsafeI 0

instance (Inhabited CIntMax l r, l <= K.P 1, K.P 1 <= r) => One CIntMax l r where
  one = UnsafeI 1

