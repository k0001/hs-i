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
  ( IntervalCtx    P.Integer ('Just l) ('Just r)
  ) => Interval    P.Integer ('Just l) ('Just r) where
  type IntervalCtx P.Integer ('Just l) ('Just r) =
    (K.KnownInteger l, K.KnownInteger r, l <= r)
  type MinI P.Integer ('Just l) ('Just r) = l
  type MaxI P.Integer ('Just l) ('Just r) = r
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x <= r)
    where l = K.integerVal (Proxy @l)
          r = K.integerVal (Proxy @r)
  negate' = from . P.negate . unwrap
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  let (q, m) = divMod (unwrap a) (unwrap b)
                  guard (m == 0)
                  from q

instance forall l.
  ( IntervalCtx    P.Integer ('Just l) 'Nothing
  ) => Interval    P.Integer ('Just l) 'Nothing where
  type IntervalCtx P.Integer ('Just l) 'Nothing = K.KnownInteger l
  type MinI P.Integer ('Just l) 'Nothing = l
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x)
    where l = K.integerVal (Proxy @l)
  negate' = from . P.negate . unwrap
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  let (q, m) = divMod (unwrap a) (unwrap b)
                  guard (m == 0)
                  from q

instance forall r.
  ( IntervalCtx    P.Integer 'Nothing ('Just r)
  ) => Interval    P.Integer 'Nothing ('Just r) where
  type IntervalCtx P.Integer 'Nothing ('Just r) = K.KnownInteger r
  type MaxI P.Integer 'Nothing ('Just r) = r
  inhabitant = max
  from = \x -> unsafest x <$ guard (x <= r)
    where r = K.integerVal (Proxy @r)
  negate' = from . P.negate . unwrap
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  let (q, m) = divMod (unwrap a) (unwrap b)
                  guard (m == 0)
                  from q

instance Interval P.Integer 'Nothing 'Nothing where
  inhabitant = zero
  from = pure . wrap
  negate' = pure . wrap . P.negate . unwrap
  a `plus'` b = pure (a `plus` b)
  a `mult'` b = pure (a `mult` b)
  a `minus'` b = pure (a `minus` b)
  a `div'` b = do guard (unwrap b /= 0)
                  let (q, m) = divMod (unwrap a) (unwrap b)
                  guard (m == 0)
                  from q

--------------------------------------------------------------------------------

instance
  ( Interval Integer ('Just l) ('Just r)
  ) => Clamp Integer ('Just l) ('Just r)

instance
  ( Interval Integer ('Just l) 'Nothing
  ) => Clamp Integer ('Just l) 'Nothing where
  clamp = \case
    x | x <= unwrap min_ -> min_
      | otherwise -> UnsafeI x
    where min_ = min

instance
  ( Interval Integer 'Nothing ('Just r)
  ) => Clamp Integer 'Nothing ('Just r) where
  clamp = \case
    x | x >= unwrap max_ -> max_
      | otherwise -> UnsafeI x
    where max_ = max

instance
  ( Interval Integer 'Nothing 'Nothing
  ) => Clamp Integer 'Nothing 'Nothing where
  clamp = UnsafeI

--------------------------------------------------------------------------------

instance
  ( lu <= ld
  , rd <= ru
  , Interval Integer ('Just ld) ('Just rd)
  , Interval Integer ('Just lu) ('Just ru)
  ) => Up    Integer ('Just ld) ('Just rd) ('Just lu) ('Just ru)

instance
  ( lu <= ld
  , Interval Integer ('Just ld) yrd
  , Interval Integer ('Just lu) 'Nothing
  ) => Up    Integer ('Just ld) yrd ('Just lu) 'Nothing

instance
  ( rd <= ru
  , Interval Integer yld ('Just rd)
  , Interval Integer 'Nothing ('Just ru)
  ) => Up    Integer yld ('Just rd) 'Nothing ('Just ru)

instance
  ( Interval Integer yld yrd
  , Interval Integer 'Nothing 'Nothing )
  => Up Integer yld yrd 'Nothing 'Nothing

--------------------------------------------------------------------------------

instance forall l r t.
  ( Interval    P.Integer ('Just l) ('Just r)
  , KnownCtx    P.Integer ('Just l) ('Just r) t
  ) => Known    P.Integer ('Just l) ('Just r) t where
  type KnownCtx P.Integer ('Just l) ('Just r) t =
    (K.KnownInteger t, l <= t, t <= r)
  known' = UnsafeI . K.integerVal

instance forall t l.
  ( Interval    P.Integer ('Just l) 'Nothing
  , KnownCtx    P.Integer ('Just l) 'Nothing t
  ) => Known    P.Integer ('Just l) 'Nothing t where
  type KnownCtx P.Integer ('Just l) 'Nothing t = (K.KnownInteger t, l <= t)
  known' = UnsafeI . K.integerVal

instance forall t r.
  ( Interval    P.Integer 'Nothing ('Just r)
  , KnownCtx    P.Integer 'Nothing ('Just r) t
  ) => Known    P.Integer 'Nothing ('Just r) t where
  type KnownCtx P.Integer 'Nothing ('Just r) t = (K.KnownInteger t, t <= r)
  known' = UnsafeI . K.integerVal

instance forall t.
  ( KnownCtx    P.Integer 'Nothing 'Nothing t
  ) => Known    P.Integer 'Nothing 'Nothing t where
  type KnownCtx P.Integer 'Nothing 'Nothing t = K.KnownInteger t
  known' = UnsafeI . K.integerVal

--------------------------------------------------------------------------------

instance forall l r.
  ( Interval P.Integer ('Just l) ('Just r)
  ) => With  P.Integer ('Just l) ('Just r) where
  with x g = case K.someIntegerVal (unwrap x) of
    K.SomeInteger (pt :: Proxy t) ->
      fromMaybe (error "I.with(Integer): impossible") $ do
        Dict <- leInteger @l @t
        Dict <- leInteger @t @r
        pure (g pt)

instance forall l.
  ( Interval P.Integer ('Just l) 'Nothing
  ) => With  P.Integer ('Just l) 'Nothing where
  with x g = case K.someIntegerVal (unwrap x) of
    K.SomeInteger (pt :: Proxy t) ->
      fromMaybe (error "I.with(Integer): impossible") $ do
        Dict <- leInteger @l @t
        pure (g pt)

instance forall r.
  ( Interval P.Integer 'Nothing ('Just r)
  ) => With  P.Integer 'Nothing ('Just r) where
  with x g = case K.someIntegerVal (unwrap x) of
    K.SomeInteger (pt :: Proxy t) ->
      fromMaybe (error "I.with(Integer): impossible") $ do
        Dict <- leInteger @t @r
        pure (g pt)

instance With P.Integer 'Nothing 'Nothing where
  with x g = case K.someIntegerVal (unwrap x) of
    K.SomeInteger (pt :: Proxy t) -> g pt

--------------------------------------------------------------------------------

instance
  ( Interval    P.Integer ('Just l) ('Just r), l /= r
  ) => Discrete P.Integer ('Just l) ('Just r) where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance
  ( Interval    P.Integer ('Just l) 'Nothing
  ) => Discrete P.Integer ('Just l) 'Nothing where
  pred' i = UnsafeI (unwrap i - 1) <$ guard (min < i)
  succ' = pure . succ

instance
  ( Interval    P.Integer 'Nothing ('Just r)
  ) => Discrete P.Integer 'Nothing ('Just r) where
  pred' = pure . pred
  succ' i = UnsafeI (unwrap i + 1) <$ guard (i < max)

instance Discrete P.Integer 'Nothing 'Nothing where
  pred'  = pure . pred
  succ'  = pure . succ

--------------------------------------------------------------------------------

instance
  ( Discrete P.Integer 'Nothing r
  ) => Pred  P.Integer 'Nothing r where
  pred i = UnsafeI (unwrap i - 1)

--------------------------------------------------------------------------------

instance
  ( Discrete P.Integer l 'Nothing
  ) => Succ  P.Integer l 'Nothing where
  succ i = UnsafeI (unwrap i + 1)

--------------------------------------------------------------------------------

instance
  ( Interval P.Integer ('Just l) 'Nothing, K.P 0 <= l
  ) => Plus  P.Integer ('Just l) 'Nothing where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

instance
  ( Interval P.Integer 'Nothing ('Just r), r <= K.P 0
  ) => Plus  P.Integer 'Nothing ('Just r) where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

instance Plus P.Integer 'Nothing 'Nothing where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance
  ( Interval P.Integer ('Just l) 'Nothing, K.P 0 <= l
  ) => Mult P.Integer ('Just l) 'Nothing where
  a `mult` b = UnsafeI (unwrap a * unwrap b)

instance Mult P.Integer 'Nothing 'Nothing where
  a `mult` b = UnsafeI (unwrap a * unwrap b)

--------------------------------------------------------------------------------

instance Minus P.Integer 'Nothing 'Nothing where
  a `minus` b = UnsafeI (unwrap a - unwrap b)

--------------------------------------------------------------------------------

instance
  ( Interval P.Integer ('Just l) ('Just r), l <= K.P 0, K.P 0 <= r
  ) => Zero  P.Integer ('Just l) ('Just r) where
  zero = UnsafeI 0

instance
  ( Interval P.Integer ('Just l) 'Nothing, l <= K.P 0
  ) => Zero  P.Integer ('Just l) 'Nothing where
  zero = UnsafeI 0

instance
  ( Interval P.Integer 'Nothing ('Just r), K.P 0 <= r
  ) => Zero  P.Integer 'Nothing ('Just r) where
  zero = UnsafeI 0

instance Zero P.Integer 'Nothing 'Nothing where
  zero = UnsafeI 0

--------------------------------------------------------------------------------

instance
  ( Interval P.Integer ('Just l) ('Just r), l <= K.P 1, K.P 1 <= r
  ) => One   P.Integer ('Just l) ('Just r) where
  one = UnsafeI 1

instance
  ( Interval P.Integer ('Just l) 'Nothing, l <= K.P 1
  ) => One   P.Integer ('Just l) 'Nothing where
  one = UnsafeI 1

instance
  ( Interval P.Integer 'Nothing ('Just r), K.P 1 <= r
  ) => One   P.Integer 'Nothing ('Just r) where
  one = UnsafeI 1

instance One P.Integer 'Nothing 'Nothing where
  one = UnsafeI 1

--------------------------------------------------------------------------------

instance
  ( Zero      P.Integer ('Just l) ('Just r), l K.== K.Negate r
  ) => Negate P.Integer ('Just l) ('Just r) where
  negate = UnsafeI . P.negate . unwrap

instance Negate P.Integer 'Nothing 'Nothing where
  negate = UnsafeI . P.negate . unwrap

--------------------------------------------------------------------------------

instance
  ( Interval Integer ('Just l) ('Just r)
  ) => Shove Integer ('Just l) ('Just r) where
  shove = \x -> unsafe $ mod x (r - l + 1) + l
    where l = unwrap (min @Integer @('Just l) @('Just r))
          r = unwrap (max @Integer @('Just l) @('Just r))

instance
  ( Interval Integer ('Just l) 'Nothing
  ) => Shove Integer ('Just l) 'Nothing where
  shove = \x -> unsafe $ if x < l then l + (l - x) else x
    where l = unwrap (min @Integer @('Just l) @'Nothing)

instance
  ( Interval Integer 'Nothing ('Just r)
  ) => Shove Integer 'Nothing ('Just r) where
  shove = \x -> unsafe $ if x > r then r - (x - r) else x
    where r = unwrap (max @Integer @'Nothing @('Just r))

instance
  ( Interval Integer 'Nothing 'Nothing
  ) => Shove Integer 'Nothing 'Nothing where
  shove = wrap

