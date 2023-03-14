{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-error=missing-methods #-}
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
import Prelude hiding (min, max, div)

import I.Internal

--------------------------------------------------------------------------------

instance forall l.
  ( IntervalCtx Natural l 'Nothing
  ) => Interval Natural l 'Nothing where
  type IntervalCtx Natural l 'Nothing = (L.KnownNat l, MinT Natural <= l)
  type MinI Natural l 'Nothing = l
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    pure (UnsafeI x)

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
  from x = do
    L.SomeNat (_ :: Proxy x) <- L.someNatVal (toInteger x)
    Dict <- leNatural @l @x
    Dict <- leNatural @x @r
    pure (UnsafeI x)

instance
  ( Interval Natural l 'Nothing, InhabitedCtx Natural l 'Nothing
  ) => Inhabited Natural l 'Nothing where
  type InhabitedCtx Natural l 'Nothing = ()
  inhabitant = min

instance
  ( Interval Natural l ('Just r), InhabitedCtx Natural l ('Just r)
  ) => Inhabited Natural l ('Just r) where
  type InhabitedCtx Natural l ('Just r) = ()
  inhabitant = min

instance forall t l.
  ( Inhabited Natural l 'Nothing, KnownCtx Natural t l 'Nothing
  ) => Known Natural t l 'Nothing where
  type KnownCtx Natural t l 'Nothing = (L.KnownNat t, l <= t)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall t l r.
  ( Inhabited Natural l ('Just r), KnownCtx Natural t l ('Just r)
  ) => Known Natural t l ('Just r) where
  type KnownCtx Natural t l ('Just r) = (L.KnownNat t, l <= t, t <= r)
  known = UnsafeI (fromInteger (L.natVal (Proxy @t)))

instance forall l. (Inhabited Natural l 'Nothing)
  => With Natural l 'Nothing where
  with x g = fromMaybe (error "I.with(Natural): impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    pure (g pt)

instance forall l r. (Inhabited Natural l ('Just r))
  => With Natural l ('Just r) where
  with x g = fromMaybe (error "I.with(Natural): impossible") $ do
    L.SomeNat (pt :: Proxy t) <- L.someNatVal (toInteger (unwrap x))
    Dict <- leNatural @l @t
    Dict <- leNatural @t @r
    pure (g pt)

instance
  ( Inhabited Natural l 'Nothing, PredCtx Natural l 'Nothing
  ) => Pred Natural l 'Nothing where
  type PredCtx Natural l 'Nothing = ()
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Natural l ('Just r), PredCtx Natural l ('Just r)
  ) => Pred Natural l ('Just r) where
  type PredCtx Natural l ('Just r) = l /= r
  pred i = UnsafeI (unwrap i - 1) <$ guard (min < i)

instance
  ( Inhabited Natural l 'Nothing, SuccCtx Natural l 'Nothing
  ) => Succ Natural l 'Nothing where
  type SuccCtx Natural l 'Nothing = ()
  succ i = pure (UnsafeI (unwrap i + 1))

instance
  ( Inhabited Natural l ('Just r), SuccCtx Natural l ('Just r)
  ) => Succ Natural l ('Just r) where
  type SuccCtx Natural l ('Just r) = l /= r
  succ i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Natural l r, PlusCtx Natural l r) => Plus Natural l r where
  type PlusCtx Natural l r = ()
  a `plus` b = from (unwrap a + unwrap b)

instance (Inhabited Natural l r, MultCtx Natural l r) => Mult Natural l r where
  type MultCtx Natural l r = ()
  a `mult` b = from (unwrap a * unwrap b)

instance (Inhabited Natural l r, MinusCtx Natural l r) => Minus Natural l r where
  type MinusCtx Natural l r = ()
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

instance (Inhabited Natural 0 r, ZeroCtx Natural 0 r) => Zero Natural 0 r where
  type ZeroCtx Natural 0 r = ()
  zero = UnsafeI 0

instance (Inhabited Natural l 'Nothing, OneCtx Natural l 'Nothing)
  => One Natural l 'Nothing where
  type OneCtx Natural l 'Nothing = l <= 1
  one = UnsafeI 1

instance (Inhabited Natural l ('Just r), OneCtx Natural l ('Just r))
  => One Natural l ('Just r) where
  type OneCtx Natural l ('Just r) = (l <= 1, 1 <= r)
  one = UnsafeI 1

