-- File generated by genmodules.sh. Do not modify.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Generated.CUChar () where

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


type instance MinL CUChar = MinT CUChar
type instance MaxR CUChar = MaxT CUChar

instance forall l r.
  ( IntervalCtx CUChar l r
  ) => Interval CUChar l r where
  type IntervalCtx CUChar l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT CUChar <= l
    , l <= r
    , r <= MaxT CUChar )
  type MinI CUChar l r = l
  type MaxI CUChar l r = r

instance
  ( Interval CUChar l r, InhabitedCtx CUChar l r
  ) => Inhabited CUChar l r where
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

instance (Inhabited CUChar l r) => Clamp CUChar l r

instance (Inhabited CUChar ld rd, Inhabited CUChar lu ru, lu <= ld, rd <= ru)
  => Up CUChar ld rd lu ru

instance forall t l r.
  ( Inhabited CUChar l r, KnownCtx CUChar t l r
  ) => Known CUChar t l r where
  type KnownCtx CUChar t l r = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l r. (Inhabited CUChar l r) => With CUChar l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited CUChar l r, l /= r
  ) => Discrete CUChar l r where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited CUChar 0 r) => Zero CUChar 0 r where
  zero = UnsafeI 0

instance (Inhabited CUChar l r, l <= 1, 1 <= r) => One CUChar l r where
  one = UnsafeI 1

instance forall l r. (Inhabited CUChar l r) => Shove CUChar l r where
  shove = \x -> UnsafeI $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @CUChar @l @r))
          r = toInteger (unwrap (max @CUChar @l @r))

