{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Natural () where

import Control.Monad
import Data.Bits
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Data.Type.Ord
import Numeric.Natural (Natural)
import GHC.TypeLits qualified as L
import KindInteger (type (/=))
import Prelude hiding (min, max, div, succ)

import I.Internal

--------------------------------------------------------------------------------

type instance MinL Natural = MinT Natural
type instance MaxR Natural = 'Nothing

--------------------------------------------------------------------------------

instance forall l r.
  ( IntervalCtx Natural l ('Just r)
  ) => Interval Natural l ('Just r) where
  type IntervalCtx Natural l ('Just r) =
    ( L.KnownNat l
    , L.KnownNat r
    , MinT Natural <= l
    , l <= r )
  type MinI Natural l ('Just r) = l
  type MaxI Natural l ('Just r) = r

instance forall l.
  ( IntervalCtx Natural l 'Nothing
  ) => Interval Natural l 'Nothing where
  type IntervalCtx Natural l 'Nothing = (L.KnownNat l, MinT Natural <= l)
  type MinI Natural l 'Nothing = l

--------------------------------------------------------------------------------

instance
  ( Interval Natural l ('Just r), InhabitedCtx Natural l ('Just r)
  ) => Inhabited Natural l ('Just r) where
  type InhabitedCtx Natural l ('Just r) = ()
  inhabitant = min
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                           toInteger (unwrap b))
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance
  ( Interval Natural l 'Nothing
  , InhabitedCtx Natural l 'Nothing
  ) => Inhabited Natural l 'Nothing where
  type InhabitedCtx Natural l 'Nothing = ()
  inhabitant = min
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    pure (UnsafeI x)
  a `plus'` b = pure (a `plus` b)
  a `mult'` b = pure (a `mult` b)
  a `minus'` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                           toInteger (unwrap b))
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

--------------------------------------------------------------------------------

instance forall t l r.
  ( Inhabited Natural l ('Just r), KnownCtx Natural t l ('Just r)
  ) => Known Natural t l ('Just r) where
  type KnownCtx Natural t l ('Just r) = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall t l.
  ( Inhabited Natural l 'Nothing, KnownCtx Natural t l 'Nothing
  ) => Known Natural t l 'Nothing where
  type KnownCtx Natural t l 'Nothing = (L.KnownNat t, l <= t)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

--------------------------------------------------------------------------------

instance forall l r. (Inhabited Natural l ('Just r))
  => With Natural l ('Just r) where
  with x g = fromMaybe (error "I.with(Natural): impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance forall l. (Inhabited Natural l 'Nothing)
  => With Natural l 'Nothing where
  with x g = fromMaybe (error "I.with(Natural): impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    pure (g pt)

--------------------------------------------------------------------------------

instance (Inhabited Natural l ('Just r), l /= r)
  => Discrete Natural l ('Just r) where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Natural l 'Nothing) => Discrete Natural l 'Nothing where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' = pure . succ

--------------------------------------------------------------------------------

instance (Inhabited Natural l 'Nothing) => Plus Natural l 'Nothing where
  plus a b = UnsafeI (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance (Inhabited Natural l 'Nothing) => Mult Natural l 'Nothing where
  mult a b = UnsafeI (unwrap a * unwrap b)

--------------------------------------------------------------------------------

instance (Discrete Natural l 'Nothing) => Succ Natural l 'Nothing where
  succ i = UnsafeI (unwrap i + 1)

--------------------------------------------------------------------------------

instance (Inhabited Natural 0 r) => Zero Natural 0 r where
  zero = UnsafeI 0

--------------------------------------------------------------------------------

instance (Inhabited Natural l 'Nothing, l <= 1) => One Natural l 'Nothing where
  one = UnsafeI 1

instance (Inhabited Natural l ('Just r), l <= 1, 1 <= r)
  => One Natural l ('Just r) where
  one = UnsafeI 1

