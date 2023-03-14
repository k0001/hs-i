{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-error=missing-methods #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Integer () where

import Control.Monad
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Data.Type.Ord
import KindInteger qualified as K
import KindInteger (type (/=))
import Prelude hiding (min, max, div, succ, pred)
import Prelude qualified as P

import I.Internal

--------------------------------------------------------------------------------

type instance MinL P.Integer = 'Nothing
type instance MaxR P.Integer = 'Nothing

instance forall l r.
  ( IntervalCtx P.Integer ('Just l) ('Just r)
  ) => Interval P.Integer ('Just l) ('Just r) where
  type IntervalCtx P.Integer ('Just l) ('Just r) =
    (K.KnownInteger l, K.KnownInteger r, l <= r)
  type MinI P.Integer ('Just l) ('Just r) = l
  type MaxI P.Integer ('Just l) ('Just r) = r

instance forall l.
  ( IntervalCtx P.Integer ('Just l) 'Nothing
  ) => Interval P.Integer ('Just l) 'Nothing where
  type IntervalCtx P.Integer ('Just l) 'Nothing = K.KnownInteger l
  type MinI P.Integer ('Just l) 'Nothing = l

instance forall r.
  ( IntervalCtx P.Integer 'Nothing ('Just r)
  ) => Interval P.Integer 'Nothing ('Just r) where
  type IntervalCtx P.Integer 'Nothing ('Just r) = K.KnownInteger r
  type MaxI P.Integer 'Nothing ('Just r) = r

instance Interval P.Integer 'Nothing 'Nothing

--------------------------------------------------------------------------------

instance forall l r.
  ( Interval P.Integer ('Just l) ('Just r)
  , InhabitedCtx P.Integer ('Just l) ('Just r)
  ) => Inhabited P.Integer ('Just l) ('Just r) where
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy x) <- K.someIntegerVal x = do
    Dict <- leInteger @l @x
    Dict <- leInteger @x @r
    pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' _ = Nothing
  a `plus` b = from (unwrap a + unwrap b)
  a `mult` b = from (unwrap a * unwrap b)
  a `minus` b = from (unwrap a - unwrap b)

instance forall l.
  ( Interval P.Integer ('Just l) 'Nothing
  , InhabitedCtx P.Integer ('Just l) 'Nothing
  ) => Inhabited P.Integer ('Just l) 'Nothing where
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy x) <- K.someIntegerVal x = do
    Dict <- leInteger @l @x
    pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' _ = Nothing
  a `plus` b = from (unwrap a + unwrap b)
  a `mult` b = from (unwrap a * unwrap b)
  a `minus` b = from (unwrap a - unwrap b)

instance forall r.
  ( Interval P.Integer 'Nothing ('Just r)
  , InhabitedCtx P.Integer 'Nothing ('Just r)
  ) => Inhabited P.Integer 'Nothing ('Just r) where
  inhabitant = max
  from x | K.SomeInteger (_ :: Proxy x) <- K.someIntegerVal x = do
    Dict <- leInteger @x @r
    pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' _ = Nothing
  a `plus` b = from (unwrap a + unwrap b)
  a `mult` b = from (unwrap a * unwrap b)
  a `minus` b = from (unwrap a - unwrap b)

instance Inhabited P.Integer 'Nothing 'Nothing where
  inhabitant = zero
  from = pure . wrap
  negate' = pure . wrap . P.negate . unwrap
  recip' _ = Nothing
  a `plus` b = pure (wrap (unwrap a + unwrap b))
  a `mult` b = pure (wrap (unwrap a * unwrap b))
  a `minus` b = pure (wrap (unwrap a - unwrap b))

--------------------------------------------------------------------------------

instance forall t l r.
  ( Inhabited P.Integer ('Just l) ('Just r)
  , KnownCtx P.Integer t ('Just l) ('Just r)
  ) => Known P.Integer t ('Just l) ('Just r) where
  type KnownCtx P.Integer t ('Just l) ('Just r) =
    (K.KnownInteger t, l <= t, t <= r)
  known = UnsafeI (K.integerVal (Proxy @t))

instance forall t l.
  ( Inhabited P.Integer ('Just l) 'Nothing
  , KnownCtx P.Integer t ('Just l) 'Nothing
  ) => Known P.Integer t ('Just l) 'Nothing where
  type KnownCtx P.Integer t ('Just l) 'Nothing = (K.KnownInteger t, l <= t)
  known = UnsafeI (K.integerVal (Proxy @t))

instance forall t r.
  ( Inhabited P.Integer 'Nothing ('Just r)
  , KnownCtx P.Integer t 'Nothing ('Just r)
  ) => Known P.Integer t 'Nothing ('Just r) where
  type KnownCtx P.Integer t 'Nothing ('Just r) = (K.KnownInteger t, t <= r)
  known = UnsafeI (K.integerVal (Proxy @t))

instance forall t.
  ( KnownCtx P.Integer t 'Nothing 'Nothing
  ) => Known P.Integer t 'Nothing 'Nothing where
  type KnownCtx P.Integer t 'Nothing 'Nothing = K.KnownInteger t
  known = UnsafeI (K.integerVal (Proxy @t))

--------------------------------------------------------------------------------

instance forall l r.
  ( Inhabited P.Integer ('Just l) ('Just r)
  ) => With P.Integer ('Just l) ('Just r) where
  with x g | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (unwrap x) =
    fromMaybe (error "I.with(Integer): impossible") $ do
      Dict <- leInteger @l @t
      Dict <- leInteger @t @r
      pure (g pt)

instance forall l.
  ( Inhabited P.Integer ('Just l) 'Nothing
  ) => With P.Integer ('Just l) 'Nothing where
  with x g | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (unwrap x) =
    fromMaybe (error "I.with(Integer): impossible") $ do
      Dict <- leInteger @l @t
      pure (g pt)

instance forall r.
  ( Inhabited P.Integer 'Nothing ('Just r)
  ) => With P.Integer 'Nothing ('Just r) where
  with x g | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (unwrap x) =
    fromMaybe (error "I.with(Integer): impossible") $ do
      Dict <- leInteger @t @r
      pure (g pt)

instance With P.Integer 'Nothing 'Nothing where
  with x g | K.SomeInteger (pt :: Proxy t) <- K.someIntegerVal (unwrap x) = g pt

--------------------------------------------------------------------------------

instance (Inhabited P.Integer ('Just l) ('Just r), l /= r)
  => Discrete P.Integer ('Just l) ('Just r) where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance (Inhabited P.Integer ('Just l) 'Nothing)
  => Discrete P.Integer ('Just l) 'Nothing where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' = pure . succ

instance (Inhabited P.Integer 'Nothing ('Just r))
  => Discrete P.Integer 'Nothing ('Just r) where
  pred' = pure . pred
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance Discrete P.Integer 'Nothing 'Nothing where
  pred'  = pure . pred
  succ'  = pure . succ

--------------------------------------------------------------------------------

instance (Discrete P.Integer 'Nothing r) => Pred P.Integer 'Nothing r where
  pred i = UnsafeI (unwrap i - 1)

--------------------------------------------------------------------------------

instance (Discrete P.Integer l 'Nothing) => Succ P.Integer l 'Nothing where
  succ i = UnsafeI (unwrap i + 1)

--------------------------------------------------------------------------------

instance (Inhabited P.Integer ('Just l) ('Just r), l <= K.P 0, K.P 0 <= r)
  => Zero P.Integer ('Just l) ('Just r) where
  zero = UnsafeI 0

instance (Inhabited P.Integer ('Just l) 'Nothing, l <= K.P 0)
  => Zero P.Integer ('Just l) 'Nothing where
  zero = UnsafeI 0

instance (Inhabited P.Integer 'Nothing ('Just r), K.P 0 <= r)
  => Zero P.Integer 'Nothing ('Just r) where
  zero = UnsafeI 0

instance Zero P.Integer 'Nothing 'Nothing where
  zero = UnsafeI 0

--------------------------------------------------------------------------------

instance (Inhabited P.Integer ('Just l) ('Just r), l <= K.P 1, K.P 1 <= r)
  => One P.Integer ('Just l) ('Just r) where
  one = UnsafeI 1

instance (Inhabited P.Integer ('Just l) 'Nothing, l <= K.P 1)
  => One P.Integer ('Just l) 'Nothing where
  one = UnsafeI 1

instance (Inhabited P.Integer 'Nothing ('Just r), K.P 1 <= r)
  => One P.Integer 'Nothing ('Just r) where
  one = UnsafeI 1

instance One P.Integer 'Nothing 'Nothing where
  one = UnsafeI 1

--------------------------------------------------------------------------------

instance (Zero P.Integer ('Just l) ('Just r), l K.== K.Negate r)
  => Negate P.Integer ('Just l) ('Just r) where
  negate = UnsafeI . P.negate . unwrap

instance Negate P.Integer 'Nothing 'Nothing where
  negate = UnsafeI . P.negate . unwrap

