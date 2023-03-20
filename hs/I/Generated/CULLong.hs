-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CULLong () where

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


type instance MinL CULLong = MinT CULLong
type instance MaxR CULLong = MaxT CULLong

instance forall l r.
  ( IntervalCtx CULLong l r
  ) => Interval CULLong l r where
  type IntervalCtx CULLong l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CULLong <= l
    , l <= r
    , r <= MaxT CULLong )
  type MinI CULLong l r = l
  type MaxI CULLong l r = r

instance
  ( Interval CULLong l r, InhabitedCtx CULLong l r
  ) => Inhabited CULLong l r where
  inhabitant = min
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

  a `plus'` b = do
    let x = unwrap a + unwrap b
    guard (x >= unwrap a)
    from x

  a `mult'` b = do
    x <- toIntegralSized (toInteger (unwrap a) * toInteger (unwrap b))
    from x

  a `minus'` b = do
    guard (a >= b)
    from (unwrap a - unwrap b)

  a `div'` b = do
    guard (unwrap b /= 0)
    (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
    from q

instance (Inhabited CULLong l r) => Clamp CULLong l r

instance (Inhabited CULLong ld rd, Inhabited CULLong lu ru, lu <= ld, rd <= ru)
  => Up CULLong ld rd lu ru

instance forall l r t.
  ( Inhabited CULLong l r, KnownCtx CULLong l r t
  ) => Known CULLong l r t where
  type KnownCtx CULLong l r t = (L.KnownNat t, l <= t, t <= r)
  known' = UnsafeI . fromInteger . L.natVal

instance forall l r. (Inhabited CULLong l r) => With CULLong l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CULLong l r, l /= r
  ) => Discrete CULLong l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CULLong 0 r) => Zero CULLong 0 r where
  zero = UnsafeI 0

instance (Inhabited CULLong l r, l <= 1, 1 <= r) => One CULLong l r where
  one = UnsafeI 1

instance forall l r. (Inhabited CULLong l r) => Shove CULLong l r where
  shove = \x -> UnsafeI $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CULLong @l @r))
          r = toInteger (unwrap (max @CULLong @l @r))

