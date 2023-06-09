{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module I.Rational () where

import Control.Monad
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Data.Type.Ord
import GHC.TypeLits qualified as L
import GHC.Real
import KindRational (type (/))
import KindRational qualified as KR
import Prelude hiding (min, max, div, succ, pred)
import Prelude qualified as P
import Unsafe.Coerce (unsafeCoerce)

import I.Internal

--------------------------------------------------------------------------------

type instance MinL P.Rational = 'Nothing
type instance MaxR P.Rational = 'Nothing

instance forall l r.
  ( IntervalCtx    P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => Interval    P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  type IntervalCtx P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) =
    (KR.KnownRational l, KR.KnownRational r, l <= r)
  type MinI P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) = l
  type MaxI P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) = r
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x <= r)
    where l = KR.rationalVal (Proxy @l)
          r = KR.rationalVal (Proxy @r)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l r.
  ( IntervalCtx    P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => Interval    P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  type IntervalCtx P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) =
    (KR.KnownRational l, KR.KnownRational r, l < r)
  type MinI P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) = l
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x < r)
    where l = KR.rationalVal (Proxy @l)
          r = KR.rationalVal (Proxy @r)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l.
  ( IntervalCtx    P.Rational ('Just '( 'True, l)) 'Nothing
  ) => Interval    P.Rational ('Just '( 'True, l)) 'Nothing where
  type IntervalCtx P.Rational ('Just '( 'True, l)) 'Nothing = KR.KnownRational l
  type MinI P.Rational ('Just '( 'True, l)) 'Nothing = l
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x)
    where l = KR.rationalVal (Proxy @l)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)


instance forall l r.
  ( IntervalCtx    P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => Interval    P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  type IntervalCtx P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) =
    (KR.KnownRational l, KR.KnownRational r, l < r)
  type MaxI P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) = r
  inhabitant = max
  from = \x -> unsafest x <$ guard (l < x && x <= r)
    where l = KR.rationalVal (Proxy @l)
          r = KR.rationalVal (Proxy @r)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall r.
  ( IntervalCtx    P.Rational 'Nothing ('Just '( 'True, r))
  ) => Interval    P.Rational 'Nothing ('Just '( 'True, r)) where
  type IntervalCtx P.Rational 'Nothing ('Just '( 'True, r)) = KR.KnownRational r
  type MaxI P.Rational 'Nothing ('Just '( 'True, r)) = r
  inhabitant = max
  from = \x -> unsafest x <$ guard (x <= r)
    where r = KR.rationalVal (Proxy @r)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l r.
  ( IntervalCtx    P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => Interval    P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  type IntervalCtx P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) =
    (KR.KnownRational l, KR.KnownRational r, l < r)
  inhabitant = -- halfway between l and r
    let l' = KR.rationalVal (Proxy @l)
        r' = KR.rationalVal (Proxy @r)
    in unsafe (l' + (r' - l') / 2)
  from = \x -> unsafest x <$ guard (l < x && x < r)
    where l = KR.rationalVal (Proxy @l)
          r = KR.rationalVal (Proxy @r)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall r.
  ( IntervalCtx    P.Rational 'Nothing ('Just '( 'False, r))
  ) => Interval    P.Rational 'Nothing ('Just '( 'False, r)) where
  type IntervalCtx P.Rational 'Nothing ('Just '( 'False, r)) =
    KR.KnownRational r
  inhabitant = unsafe (KR.rationalVal (Proxy @r) - 1)
  from = \x -> unsafest x <$ guard (x < r)
    where r = KR.rationalVal (Proxy @r)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l.
  ( IntervalCtx    P.Rational ('Just '( 'False, l)) 'Nothing
  ) => Interval    P.Rational ('Just '( 'False, l)) 'Nothing where
  type IntervalCtx P.Rational ('Just '( 'False, l)) 'Nothing =
    KR.KnownRational l
  inhabitant = unsafe (KR.rationalVal (Proxy @l) + 1)
  from = \x -> unsafest x <$ guard (l < x)
    where l = KR.rationalVal (Proxy @l)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance Interval P.Rational 'Nothing 'Nothing where
  inhabitant = zero
  from = pure . wrap
  negate' = pure . wrap . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = pure (a `plus` b)
  a `mult'` b = pure (a `mult` b)
  a `minus'` b = pure (a `minus` b)
  a `div'` b = do guard (unwrap b /= 0)
                  pure (wrap (unwrap a / unwrap b))

--------------------------------------------------------------------------------

instance (Interval Rational ('Just '( 'True, l)) ('Just '( 'True, r)))
  => Clamp         Rational ('Just '( 'True, l)) ('Just '( 'True, r))

instance (Interval Rational ('Just '( 'True, l)) 'Nothing)
  => Clamp         Rational ('Just '( 'True, l)) 'Nothing where
  clamp = \case
    x | x <= unwrap min_ -> min_
      | otherwise -> unsafe x
    where min_ = min

instance (Interval Rational 'Nothing ('Just '( 'True, r)))
  => Clamp         Rational 'Nothing ('Just '( 'True, r)) where
  clamp = \case
    x | x >= unwrap max_ -> max_
      | otherwise -> unsafe x
    where max_ = max

instance (Interval Rational 'Nothing 'Nothing)
  => Clamp         Rational 'Nothing 'Nothing where
  clamp = unsafe

--------------------------------------------------------------------------------

-- OO
instance
  ( Interval Rational ('Just '( 'False, ld)) ('Just '( 'False, rd))
  , Interval Rational ('Just '( 'False, lu)) ('Just '( 'False, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( 'False, ld)) ('Just '( 'False, rd))
                 ('Just '( 'False, lu)) ('Just '( 'False, ru))

-- OC
instance
  ( Interval Rational ('Just '( 'False, ld)) ('Just '( ird  , rd))
  , Interval Rational ('Just '( 'False, lu)) ('Just '( 'True, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( 'False, ld)) ('Just '( ird  , rd))
                 ('Just '( 'False, lu)) ('Just '( 'True, ru))

-- OU
instance
  ( Interval Rational ('Just '( 'False, ld)) yrd
  , Interval Rational ('Just '( 'False, lu)) 'Nothing
  , lu <= ld )
  => Up Rational ('Just '( 'False, ld)) yrd
                 ('Just '( 'False, lu)) 'Nothing

-- CO
instance
  ( Interval Rational ('Just '( ild  , ld)) ('Just '( 'False, rd))
  , Interval Rational ('Just '( 'True, lu)) ('Just '( 'False, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( ild  , ld)) ('Just '( 'False, rd))
                 ('Just '( 'True, lu)) ('Just '( 'False, ru))

-- CC
instance
  ( Interval Rational ('Just '( ild  , ld)) ('Just '( ird  , rd))
  , Interval Rational ('Just '( 'True, lu)) ('Just '( 'True, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( ild  , ld)) ('Just '( ird  , rd))
                 ('Just '( 'True, lu)) ('Just '( 'True, ru))

-- CU
instance
  ( Interval Rational ('Just '( ild  , ld)) yrd
  , Interval Rational ('Just '( 'True, lu)) 'Nothing
  , lu <= ld )
  => Up Rational ('Just '( ild  , ld)) yrd
                 ('Just '( 'True, lu)) 'Nothing

-- UO
instance
  ( Interval Rational yld      ('Just '( 'False, rd))
  , Interval Rational 'Nothing ('Just '( 'False, ru))
  , ru <= rd )
  => Up Rational yld      ('Just '( 'False, rd))
                 'Nothing ('Just '( 'False, ru))

-- UC
instance
  ( Interval Rational yld      ('Just '( ird  , rd))
  , Interval Rational 'Nothing ('Just '( 'True, ru))
  , ru <= rd )
  => Up Rational yld      ('Just '( ird  , rd))
                 'Nothing ('Just '( 'True, ru))

-- UU
instance
  ( Interval Rational yld      yrd
  , Interval Rational 'Nothing 'Nothing )
  => Up Rational yld      yrd
                 'Nothing 'Nothing

--------------------------------------------------------------------------------


instance forall t l r.
  ( Interval P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  , KnownCtx P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) t
  ) => Known P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) t where
  type KnownCtx P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) t =
    (KR.KnownRational t, l <= t, t <= r)
  known' = unsafe . KR.rationalVal

instance forall t l r.
  ( Interval P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  , KnownCtx P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) t
  ) => Known P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) t where
  type KnownCtx P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) t =
    (KR.KnownRational t, l <= t, t < r)
  known' = unsafe . KR.rationalVal

instance forall t l.
  ( Interval P.Rational ('Just '( 'True, l)) 'Nothing
  , KnownCtx P.Rational ('Just '( 'True, l)) 'Nothing t
  ) => Known P.Rational ('Just '( 'True, l)) 'Nothing t where
  type KnownCtx P.Rational ('Just '( 'True, l)) 'Nothing t =
    (KR.KnownRational t, l <= t)
  known' = unsafe . KR.rationalVal

instance forall t l r.
  ( Interval P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  , KnownCtx P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) t
  ) => Known P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) t where
  type KnownCtx P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) t =
    (KR.KnownRational t, l < t, t <= r)
  known' = unsafe . KR.rationalVal

instance forall t l.
  ( Interval P.Rational ('Just '( 'False, l)) 'Nothing
  , KnownCtx P.Rational ('Just '( 'False, l)) 'Nothing t
  ) => Known P.Rational ('Just '( 'False, l)) 'Nothing t where
  type KnownCtx P.Rational ('Just '( 'False, l)) 'Nothing t =
    (KR.KnownRational t, l < t)
  known' = unsafe . KR.rationalVal

instance forall t r.
  ( Interval P.Rational 'Nothing ('Just '( 'True, r))
  , KnownCtx P.Rational 'Nothing ('Just '( 'True, r)) t
  ) => Known P.Rational 'Nothing ('Just '( 'True, r)) t where
  type KnownCtx P.Rational 'Nothing ('Just '( 'True, r)) t =
    (KR.KnownRational t, t <= r)
  known' = unsafe . KR.rationalVal

instance forall t r.
  ( Interval P.Rational 'Nothing ('Just '( 'False, r))
  , KnownCtx P.Rational 'Nothing ('Just '( 'False, r)) t
  ) => Known P.Rational 'Nothing ('Just '( 'False, r)) t where
  type KnownCtx P.Rational 'Nothing ('Just '( 'False, r)) t =
    (KR.KnownRational t, t < r)
  known' = unsafe . KR.rationalVal

instance forall t l r.
  ( Interval P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  , KnownCtx P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) t
  ) => Known P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) t where
  type KnownCtx P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) t =
    (KR.KnownRational t, l < t, t < r)
  known' = unsafe . KR.rationalVal

instance forall t.
  ( KnownCtx P.Rational 'Nothing 'Nothing t
  ) => Known P.Rational 'Nothing 'Nothing t where
  type KnownCtx P.Rational 'Nothing 'Nothing t = KR.KnownRational t
  known' = unsafe . KR.rationalVal

--------------------------------------------------------------------------------

instance forall l r.
  ( Interval P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => With  P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- le @l @t
        Dict <- le @t @r
        pure (g pt)

instance forall l r.
  ( Interval P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => With  P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- le @l @t
        Dict <- lt @t @r
        pure (g pt)

instance forall l.
  ( Interval P.Rational ('Just '( 'True, l)) 'Nothing
  ) => With  P.Rational ('Just '( 'True, l)) 'Nothing where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- le @l @t
        pure (g pt)

instance forall l.
  ( Interval P.Rational ('Just '( 'False, l)) 'Nothing
  ) => With  P.Rational ('Just '( 'False, l)) 'Nothing where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- lt @l @t
        pure (g pt)

instance forall l r.
  ( Interval P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => With  P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- lt @l @t
        Dict <- le @t @r
        pure (g pt)

instance forall r.
  ( Interval P.Rational 'Nothing ('Just '( 'True, r))
  ) => With  P.Rational 'Nothing ('Just '( 'True, r)) where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- le @t @r
        pure (g pt)

instance forall l r.
  ( Interval P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => With  P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- lt @l @t
        Dict <- lt @t @r
        pure (g pt)

instance forall r.
  ( Interval P.Rational 'Nothing ('Just '( 'False, r))
  ) => With  P.Rational 'Nothing ('Just '( 'False, r)) where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) ->
      fromMaybe (error "I.with(Rational): impossible") $ do
        Dict <- lt @t @r
        pure (g pt)

instance With P.Rational 'Nothing 'Nothing where
  with x g = case KR.someRationalVal (unwrap x) of
    KR.SomeRational (pt :: Proxy t) -> g pt

--------------------------------------------------------------------------------

instance
  ( Interval P.Rational ('Just '(il, l)) 'Nothing, 0/1 <= l
  ) => Plus  P.Rational ('Just '(il, l)) 'Nothing where
  a `plus` b = unsafe (unwrap a + unwrap b)

instance
  ( Interval P.Rational 'Nothing ('Just '(ir, r)), r <= 0/1
  ) => Plus  P.Rational 'Nothing ('Just '(ir, r)) where
  a `plus` b = unsafe (unwrap a + unwrap b)

instance Plus P.Rational 'Nothing 'Nothing where
  a `plus` b = unsafe (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance
  ( Interval P.Rational ('Just '(il, l)) 'Nothing, 1/1 <= l
  ) => Mult  P.Rational ('Just '(il, l)) 'Nothing where
  a `mult` b = unsafe (unwrap a * unwrap b)

instance
  ( Interval P.Rational ('Just '(il, l)) ('Just '(ir, r)), 0/1 <= l, r <= 1/1
  ) => Mult  P.Rational ('Just '(il, l)) ('Just '(ir, r)) where
  a `mult` b = unsafe (unwrap a * unwrap b)

instance Mult P.Rational 'Nothing 'Nothing where
  a `mult` b = unsafe (unwrap a * unwrap b)

--------------------------------------------------------------------------------

instance
  ( 0/1 < l, r <= 1/1
  , Interval P.Rational ('Just '(il, l)) ('Just '(ir, r))
  ) => Div   P.Rational ('Just '(il, l)) ('Just '(ir, r)) where
  a `div` b = unsafe (unwrap a / unwrap b)

--------------------------------------------------------------------------------

instance Minus P.Rational 'Nothing 'Nothing where
  a `minus` b = unsafe (unwrap a - unwrap b)

--------------------------------------------------------------------------------

instance
  ( l <= 0/1, 0/1 <= r
  , Interval P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => Zero  P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  zero = unsafe 0

instance
  ( l <= 0/1, 0/1 < r
  , Interval P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => Zero  P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  zero = unsafe 0

instance
  ( l < 0/1, 0/1 <= r
  , Interval P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => Zero  P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  zero = unsafe 0

instance
  ( Interval P.Rational ('Just '( 'True, l)) 'Nothing, l <= 0/1
  ) => Zero  P.Rational ('Just '( 'True, l)) 'Nothing where
  zero = unsafe 0

instance
  ( Interval P.Rational ('Just '( 'False, l)) 'Nothing, l < 0/1
  ) => Zero P.Rational ('Just '( 'False, l)) 'Nothing where
  zero = unsafe 0

instance
  ( Interval P.Rational 'Nothing ('Just '( 'True, r)), 0/1 <= r
  ) => Zero  P.Rational 'Nothing ('Just '( 'True, r)) where
  zero = unsafe 0

instance
  ( Interval P.Rational 'Nothing ('Just '( 'False, r)), 0/1 < r
  ) => Zero  P.Rational 'Nothing ('Just '( 'False, r)) where
  zero = unsafe 0

instance
  ( l < 0/1, 0/1 < r
  , Interval P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => Zero  P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  zero = unsafe 0

instance Zero P.Rational 'Nothing 'Nothing where
  zero = unsafe 0

--------------------------------------------------------------------------------

instance
  ( l <= 1/1, 1/1 <= r
  , Interval P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => One   P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  one = unsafe 1

instance
  ( l <= 1/1, 1/1 < r
  , Interval P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => One   P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  one = unsafe 1

instance
  ( l < 1/1, 1/1 <= r
  , Interval P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => One   P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  one = unsafe 1

instance
  ( Interval P.Rational ('Just '( 'True, l)) 'Nothing, l <= 1/1
  ) => One   P.Rational ('Just '( 'True, l)) 'Nothing where
  one = unsafe 1

instance
  ( Interval P.Rational ('Just '( 'False, l)) 'Nothing, l < 1/1
  ) => One   P.Rational ('Just '( 'False, l)) 'Nothing where
  one = unsafe 1

instance
  ( Interval P.Rational 'Nothing ('Just '( 'True, r)), 1/1 <= r
  ) => One   P.Rational 'Nothing ('Just '( 'True, r)) where
  one = unsafe 1

instance
  ( Interval P.Rational 'Nothing ('Just '( 'False, r)), 1/1 < r
  ) => One   P.Rational 'Nothing ('Just '( 'False, r)) where
  one = unsafe 1

instance
  ( l < 1/1, 1/1 < r
  , Interval P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => One   P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  one = unsafe 1

instance One P.Rational 'Nothing 'Nothing where
  one = unsafe 1

--------------------------------------------------------------------------------

instance
  ( l KR.== KR.Negate r
  , Zero      P.Rational ('Just '(i, l)) ('Just '(i, r))
  ) => Negate P.Rational ('Just '(i, l)) ('Just '(i, r)) where
  negate = unsafe . P.negate . unwrap

instance Negate P.Rational 'Nothing 'Nothing where
  negate = unsafe . P.negate . unwrap

--------------------------------------------------------------------------------

instance
  ( Interval Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => Shove Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  shove | d == 0    = \_ -> min
        | otherwise = \x ->
          let t = P.min (abs (numerator x)) (abs (denominator x))
                % P.max (abs (numerator x)) (abs (denominator x))
          in unsafe $ if x < 0 then r - d * t else l + d * t
    where
      l = KR.rationalVal (Proxy @l)
      r = KR.rationalVal (Proxy @r)
      d = r - l

instance
  ( Interval Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => Shove Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  shove = \x -> let t = P.min (abs (numerator x)) (abs (denominator x))
                      % P.max (abs (numerator x)) (abs (denominator x))
                in unsafe $ if x < 0 then r1 - d1 * t else l0 + d1 * t
    where
      l0 = KR.rationalVal (Proxy @l)
      r0 = KR.rationalVal (Proxy @r)
      d0 = r0 - l0
      p0 = d0 / 1000
      r1 = r0 - p0
      d1 = r1 - l0

instance Interval Rational ('Just '( 'True, l)) 'Nothing
  => Shove Rational ('Just '( 'True, l)) 'Nothing where
  shove = \x -> unsafe (if l <= x then x else l + (l - x))
    where l = KR.rationalVal (Proxy @l)

instance
  ( Interval Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => Shove Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  shove = \x -> let t = P.min (abs (numerator x)) (abs (denominator x))
                      % P.max (abs (numerator x)) (abs (denominator x))
                in unsafe $ if x < 0 then r0 - d1 * t else l1 + d1 * t
    where
      l0 = KR.rationalVal (Proxy @l)
      r0 = KR.rationalVal (Proxy @r)
      d0 = r0 - l0
      p0 = d0 / 1000
      l1 = l0 + p0
      d1 = r0 - l1

instance
  ( Interval Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => Shove Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  shove = \x -> let t = P.min (abs (numerator x)) (abs (denominator x))
                      % P.max (abs (numerator x)) (abs (denominator x))
                in unsafe $ if x <= 0 then r1 - d1 * t else l1 + d1 * t
    where
      l0 = KR.rationalVal (Proxy @l)
      r0 = KR.rationalVal (Proxy @r)
      d0 = r0 - l0
      p0 = d0 / 1000
      l1 = l0 + p0
      r1 = r0 - p0
      d1 = r1 - l1

instance
  ( Interval Rational ('Just '( 'False, l)) 'Nothing
  ) => Shove Rational ('Just '( 'False, l)) 'Nothing where
  shove = \x -> unsafe $ if x <= l then l + (l - x) + 1 else x
    where l = KR.rationalVal (Proxy @l)

instance
  ( Interval Rational 'Nothing ('Just '( 'True, r))
  ) => Shove Rational 'Nothing ('Just '( 'True, r)) where
  shove = \x -> unsafe $ if r < x then r - (x - r) else x
    where r = KR.rationalVal (Proxy @r)

instance
  ( Interval Rational 'Nothing ('Just '( 'False, r))
  ) => Shove Rational 'Nothing ('Just '( 'False, r)) where
  shove = \x -> unsafe $ if r <= x then r - (x - r) - 1 else x
    where r = KR.rationalVal (Proxy @r)

instance Shove Rational 'Nothing 'Nothing where
  shove = unsafe

--------------------------------------------------------------------------------

lt :: forall (a :: KR.Rational) (b :: KR.Rational)
   .  (KR.KnownRational a, KR.KnownRational b)
   => Maybe (Dict (a < b))
lt = case KR.cmpRational (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Nothing
  L.GTI -> Nothing

le :: forall (a :: KR.Rational) (b :: KR.Rational)
   .  (KR.KnownRational a, KR.KnownRational b)
   => Maybe (Dict (a <= b))
le = case KR.cmpRational (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Just $ unsafeCoerce (Dict @())
  L.GTI -> Nothing

