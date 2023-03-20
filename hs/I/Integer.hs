{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
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
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance forall l.
  ( Interval P.Integer ('Just l) 'Nothing
  , InhabitedCtx P.Integer ('Just l) 'Nothing
  ) => Inhabited P.Integer ('Just l) 'Nothing where
  inhabitant = min
  from x | K.SomeInteger (_ :: Proxy x) <- K.someIntegerVal x = do
    Dict <- leInteger @l @x
    pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance forall r.
  ( Interval P.Integer 'Nothing ('Just r)
  , InhabitedCtx P.Integer 'Nothing ('Just r)
  ) => Inhabited P.Integer 'Nothing ('Just r) where
  inhabitant = max
  from x | K.SomeInteger (_ :: Proxy x) <- K.someIntegerVal x = do
    Dict <- leInteger @x @r
    pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

instance Inhabited P.Integer 'Nothing 'Nothing where
  inhabitant = zero
  from = pure . wrap
  negate' = pure . wrap . P.negate . unwrap
  a `plus'` b = pure (a `plus` b)
  a `mult'` b = pure (a `mult` b)
  a `minus'` b = pure (a `minus` b)
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

--------------------------------------------------------------------------------

instance (Inhabited Integer ('Just l) ('Just r))
  => Clamp          Integer ('Just l) ('Just r)

instance (Inhabited Integer ('Just l) 'Nothing)
  => Clamp          Integer ('Just l) 'Nothing where
  clamp = \case
    x | x <= unwrap min_ -> min_
      | otherwise -> UnsafeI x
    where min_ = min

instance (Inhabited Integer 'Nothing ('Just r))
  => Clamp          Integer 'Nothing ('Just r) where
  clamp = \case
    x | x >= unwrap max_ -> max_
      | otherwise -> UnsafeI x
    where max_ = max

instance (Inhabited Integer 'Nothing 'Nothing)
  => Clamp          Integer 'Nothing 'Nothing where
  clamp = UnsafeI

--------------------------------------------------------------------------------

instance
  ( Inhabited Integer ('Just ld) ('Just rd)
  , Inhabited Integer ('Just lu) ('Just ru)
  , lu <= ld
  , rd <= ru )
  => Up Integer ('Just ld) ('Just rd) ('Just lu) ('Just ru)

instance
  ( Inhabited Integer ('Just ld) yrd
  , Inhabited Integer ('Just lu) 'Nothing
  , lu <= ld )
  => Up Integer ('Just ld) yrd ('Just lu) 'Nothing

instance
  ( Inhabited Integer yld ('Just rd)
  , Inhabited Integer 'Nothing ('Just ru)
  , rd <= ru )
  => Up Integer yld ('Just rd) 'Nothing ('Just ru)

instance
  ( Inhabited Integer yld yrd
  , Inhabited Integer 'Nothing 'Nothing )
  => Up Integer yld yrd 'Nothing 'Nothing

--------------------------------------------------------------------------------

instance forall l r t.
  ( Inhabited P.Integer ('Just l) ('Just r)
  , KnownCtx P.Integer ('Just l) ('Just r) t
  ) => Known P.Integer ('Just l) ('Just r) t where
  type KnownCtx P.Integer ('Just l) ('Just r) t =
    (K.KnownInteger t, l <= t, t <= r)
  known' = UnsafeI . K.integerVal

instance forall t l.
  ( Inhabited P.Integer ('Just l) 'Nothing
  , KnownCtx P.Integer ('Just l) 'Nothing t
  ) => Known P.Integer ('Just l) 'Nothing t where
  type KnownCtx P.Integer ('Just l) 'Nothing t = (K.KnownInteger t, l <= t)
  known' = UnsafeI . K.integerVal

instance forall t r.
  ( Inhabited P.Integer 'Nothing ('Just r)
  , KnownCtx P.Integer 'Nothing ('Just r) t
  ) => Known P.Integer 'Nothing ('Just r) t where
  type KnownCtx P.Integer 'Nothing ('Just r) t = (K.KnownInteger t, t <= r)
  known' = UnsafeI . K.integerVal

instance forall t.
  ( KnownCtx P.Integer 'Nothing 'Nothing t
  ) => Known P.Integer 'Nothing 'Nothing t where
  type KnownCtx P.Integer 'Nothing 'Nothing t = K.KnownInteger t
  known' = UnsafeI . K.integerVal

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

instance (Inhabited P.Integer ('Just l) 'Nothing, K.P 0 <= l)
  => Plus P.Integer ('Just l) 'Nothing where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

instance (Inhabited P.Integer 'Nothing ('Just r), r <= K.P 0)
  => Plus P.Integer 'Nothing ('Just r) where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

instance Plus P.Integer 'Nothing 'Nothing where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance (Inhabited P.Integer ('Just l) 'Nothing, K.P 0 <= l)
  => Mult P.Integer ('Just l) 'Nothing where
  a `mult` b = UnsafeI (unwrap a * unwrap b)

instance Mult P.Integer 'Nothing 'Nothing where
  a `mult` b = UnsafeI (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance Minus P.Integer 'Nothing 'Nothing where
  a `minus` b = UnsafeI (unwrap a - unwrap b)

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

--------------------------------------------------------------------------------

instance Inhabited Integer ('Just l) ('Just r)
  => Shove Integer ('Just l) ('Just r) where
  shove = \x -> fromMaybe (error "shove(Integer): impossible") $
                  from $ mod x (r - l + 1) + l
    where l = unwrap (min @Integer @('Just l) @('Just r))
          r = unwrap (max @Integer @('Just l) @('Just r))

instance Inhabited Integer ('Just l) 'Nothing
  => Shove Integer ('Just l) 'Nothing where
   shove = \x -> fromMaybe (error "shove(Integer): impossible") $
                   from $ if x < l then l + (l - x) else x
     where l = unwrap (min @Integer @('Just l) @'Nothing)

instance Inhabited Integer 'Nothing ('Just r)
  => Shove Integer 'Nothing ('Just r) where
   shove = \x -> fromMaybe (error "shove(Integer): impossible") $
                   from $ if x > r then r - (x - r) else x
     where r = unwrap (max @Integer @'Nothing @('Just r))

instance Inhabited Integer 'Nothing 'Nothing
  => Shove Integer 'Nothing 'Nothing where
   shove = wrap
