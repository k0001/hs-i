{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Word8 () where

import Control.Monad
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


type instance MinL Word8 = MinT Word8
type instance MaxR Word8 = MaxT Word8

instance forall l r.
  ( IntervalCtx Word8 l r
  ) => Interval Word8 l r where
  type IntervalCtx Word8 l r =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT Word8 <= l
    , l <= r
    , r <= MaxT Word8 )
  type MinI Word8 l r = l
  type MaxI Word8 l r = r

instance
  ( Interval Word8 l r, InhabitedCtx Word8 l r
  ) => Inhabited Word8 l r where
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x <= r)
    where l = fromInteger (L.natVal (Proxy @l)) :: Word8
          r = fromInteger (L.natVal (Proxy @r)) :: Word8

  (unwrap -> a) `plus'` (unwrap -> b) = do
    guard (b <= maxBound - a)
    from (a + b)

  (unwrap -> a) `mult'` (unwrap -> b) = do
    guard (b == 0 || a <= maxBound `quot` b)
    from (a * b)

  (unwrap -> a) `minus'` (unwrap -> b) = do
    guard (b <= a)
    from (a - b)

  (unwrap -> a) `div'` (unwrap -> b) = do
    guard (b /= 0)
    let (q, m) = divMod a b
    guard (m == 0)
    from q

instance (Inhabited Word8 l r) => Clamp Word8 l r

instance (Inhabited Word8 ld rd, Inhabited Word8 lu ru, lu <= ld, rd <= ru)
  => Up Word8 ld rd lu ru

instance forall l r t.
  ( Inhabited Word8 l r, KnownCtx Word8 l r t
  ) => Known Word8 l r t where
  type KnownCtx Word8 l r t = (L.KnownNat t, l <= t, t <= r)
  known' = unsafe . fromInteger . L.natVal

instance forall l r. (Inhabited Word8 l r) => With Word8 l r where
  with x g = fromMaybe (error "I.with: impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance (Inhabited Word8 l r, l /= r) => Discrete Word8 l r where
  pred' i = unsafe (unwrap i - 1) <$ guard (min < i)
  succ' i = unsafe (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Word8 0 r) => Zero Word8 0 r where
  zero = unsafe 0

instance (Inhabited Word8 l r, l <= 1, 1 <= r) => One Word8 l r where
  one = unsafe 1

instance forall l r. (Inhabited Word8 l r) => Shove Word8 l r where
  shove = \x -> unsafe $ fromInteger (mod (toInteger x) (r - l + 1) + l)
    where l = toInteger (unwrap (min @Word8 @l @r))
          r = toInteger (unwrap (max @Word8 @l @r))

