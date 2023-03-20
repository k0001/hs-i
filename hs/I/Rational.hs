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
import GHC.Real (Ratio((:%)))
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
  ( IntervalCtx P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => Interval P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  type IntervalCtx P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) =
    (KR.KnownRational l, KR.KnownRational r, l <= r)
  type MinI P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) = l
  type MaxI P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) = r

instance forall l r.
  ( IntervalCtx P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => Interval P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  type IntervalCtx P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) =
    (KR.KnownRational l, KR.KnownRational r, l <= r)
  type MinI P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) = l

instance forall l.
  ( IntervalCtx P.Rational ('Just '( 'True, l)) 'Nothing
  ) => Interval P.Rational ('Just '( 'True, l)) 'Nothing where
  type IntervalCtx P.Rational ('Just '( 'True, l)) 'Nothing = KR.KnownRational l
  type MinI P.Rational ('Just '( 'True, l)) 'Nothing = l

instance forall l r.
  ( IntervalCtx P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => Interval P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  type IntervalCtx P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) =
    (KR.KnownRational l, KR.KnownRational r, l <= r)
  type MaxI P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) = r

instance forall r.
  ( IntervalCtx P.Rational 'Nothing ('Just '( 'True, r))
  ) => Interval P.Rational 'Nothing ('Just '( 'True, r)) where
  type IntervalCtx P.Rational 'Nothing ('Just '( 'True, r)) = KR.KnownRational r
  type MaxI P.Rational 'Nothing ('Just '( 'True, r)) = r

instance forall l r.
  ( IntervalCtx P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => Interval P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  type IntervalCtx P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) =
    (KR.KnownRational l, KR.KnownRational r, l <= r)

instance forall r.
  ( IntervalCtx P.Rational 'Nothing ('Just '( 'False, r))
  ) => Interval P.Rational 'Nothing ('Just '( 'False, r)) where
  type IntervalCtx P.Rational 'Nothing ('Just '( 'False, r)) =
    KR.KnownRational r

instance forall l.
  ( IntervalCtx P.Rational ('Just '( 'False, l)) 'Nothing
  ) => Interval P.Rational ('Just '( 'False, l)) 'Nothing where
  type IntervalCtx P.Rational ('Just '( 'False, l)) 'Nothing =
    KR.KnownRational l

instance Interval P.Rational 'Nothing 'Nothing

--------------------------------------------------------------------------------

instance forall l r.
  ( Interval P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  , InhabitedCtx P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  inhabitant = min
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- le @l @t
        Dict <- le @t @r
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l r.
  ( Interval P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  , InhabitedCtx P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  type InhabitedCtx P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) =
    l < r
  inhabitant = min
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- le @l @t
        Dict <- lt @t @r
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l.
  ( Interval P.Rational ('Just '( 'True, l)) 'Nothing
  , InhabitedCtx P.Rational ('Just '( 'True, l)) 'Nothing
  ) => Inhabited P.Rational ('Just '( 'True, l)) 'Nothing where
  inhabitant = min
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- le @l @t
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l r.
  ( Interval P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  , InhabitedCtx P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  type InhabitedCtx P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) =
    l < r
  inhabitant = max
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- lt @l @t
        Dict <- le @t @r
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall r.
  ( Interval P.Rational 'Nothing ('Just '( 'True, r))
  , InhabitedCtx P.Rational 'Nothing ('Just '( 'True, r))
  ) => Inhabited P.Rational 'Nothing ('Just '( 'True, r)) where
  inhabitant = max
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- le @t @r
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l r.
  ( Interval P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  , InhabitedCtx P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  type InhabitedCtx P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) =
    l < r
  inhabitant = -- halfway between l and r
    let l' = KR.rationalVal (Proxy @l)
        r' = KR.rationalVal (Proxy @r)
    in UnsafeI (l' + (r' - l') / 2)
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- lt @l @t
        Dict <- lt @t @r
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall r.
  ( Interval P.Rational 'Nothing ('Just '( 'False, r))
  , InhabitedCtx P.Rational 'Nothing ('Just '( 'False, r))
  ) => Inhabited P.Rational 'Nothing ('Just '( 'False, r)) where
  inhabitant = UnsafeI (KR.rationalVal (Proxy @r) - 1)
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- lt @t @r
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance forall l.
  ( Interval P.Rational ('Just '( 'False, l)) 'Nothing
  , InhabitedCtx P.Rational ('Just '( 'False, l)) 'Nothing
  ) => Inhabited P.Rational ('Just '( 'False, l)) 'Nothing where
  inhabitant = UnsafeI (KR.rationalVal (Proxy @l) + 1)
  from x0 = do
    x <- fmap KR.toPrelude (KR.fromPrelude x0)
    case KR.someRationalVal x of
      KR.SomeRational (_ :: Proxy t) -> do
        Dict <- lt @l @t
        pure (UnsafeI x)
  negate' = from . P.negate . unwrap
  recip' x = case unwrap x of n :% d -> from (d :% n)
  a `plus'` b = from (unwrap a + unwrap b)
  a `mult'` b = from (unwrap a * unwrap b)
  a `minus'` b = from (unwrap a - unwrap b)
  a `div'` b = do guard (unwrap b /= 0)
                  from (unwrap a / unwrap b)

instance Inhabited P.Rational 'Nothing 'Nothing where
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

instance (Inhabited Rational ('Just '( 'True, l)) ('Just '( 'True, r)))
  => Clamp          Rational ('Just '( 'True, l)) ('Just '( 'True, r))

instance (Inhabited Rational ('Just '( 'True, l)) 'Nothing)
  => Clamp          Rational ('Just '( 'True, l)) 'Nothing where
  clamp = \case
    x | x <= unwrap min_ -> min_
      | otherwise -> UnsafeI x
    where min_ = min

instance (Inhabited Rational 'Nothing ('Just '( 'True, r)))
  => Clamp          Rational 'Nothing ('Just '( 'True, r)) where
  clamp = \case
    x | x >= unwrap max_ -> max_
      | otherwise -> UnsafeI x
    where max_ = max

instance (Inhabited Rational 'Nothing 'Nothing)
  => Clamp          Rational 'Nothing 'Nothing where
  clamp = UnsafeI

--------------------------------------------------------------------------------

-- OO
instance
  ( Inhabited Rational ('Just '( 'False, ld)) ('Just '( 'False, rd))
  , Inhabited Rational ('Just '( 'False, lu)) ('Just '( 'False, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( 'False, ld)) ('Just '( 'False, rd))
                 ('Just '( 'False, lu)) ('Just '( 'False, ru))

-- OC
instance
  ( Inhabited Rational ('Just '( 'False, ld)) ('Just '( ird  , rd))
  , Inhabited Rational ('Just '( 'False, lu)) ('Just '( 'True, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( 'False, ld)) ('Just '( ird  , rd))
                 ('Just '( 'False, lu)) ('Just '( 'True, ru))

-- OU
instance
  ( Inhabited Rational ('Just '( 'False, ld)) yrd
  , Inhabited Rational ('Just '( 'False, lu)) 'Nothing
  , lu <= ld )
  => Up Rational ('Just '( 'False, ld)) yrd
                 ('Just '( 'False, lu)) 'Nothing

-- CO
instance
  ( Inhabited Rational ('Just '( ild  , ld)) ('Just '( 'False, rd))
  , Inhabited Rational ('Just '( 'True, lu)) ('Just '( 'False, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( ild  , ld)) ('Just '( 'False, rd))
                 ('Just '( 'True, lu)) ('Just '( 'False, ru))

-- CC
instance
  ( Inhabited Rational ('Just '( ild  , ld)) ('Just '( ird  , rd))
  , Inhabited Rational ('Just '( 'True, lu)) ('Just '( 'True, ru))
  , lu <= ld
  , rd <= ru )
  => Up Rational ('Just '( ild  , ld)) ('Just '( ird  , rd))
                 ('Just '( 'True, lu)) ('Just '( 'True, ru))

-- CU
instance
  ( Inhabited Rational ('Just '( ild  , ld)) yrd
  , Inhabited Rational ('Just '( 'True, lu)) 'Nothing
  , lu <= ld )
  => Up Rational ('Just '( ild  , ld)) yrd
                 ('Just '( 'True, lu)) 'Nothing

-- UO
instance
  ( Inhabited Rational yld      ('Just '( 'False, rd))
  , Inhabited Rational 'Nothing ('Just '( 'False, ru))
  , ru <= rd )
  => Up Rational yld      ('Just '( 'False, rd))
                 'Nothing ('Just '( 'False, ru))

-- UC
instance
  ( Inhabited Rational yld      ('Just '( ird  , rd))
  , Inhabited Rational 'Nothing ('Just '( 'True, ru))
  , ru <= rd )
  => Up Rational yld      ('Just '( ird  , rd))
                 'Nothing ('Just '( 'True, ru))

-- UU
instance
  ( Inhabited Rational yld      yrd
  , Inhabited Rational 'Nothing 'Nothing )
  => Up Rational yld      yrd
                 'Nothing 'Nothing

--------------------------------------------------------------------------------


instance forall t l r.
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  , KnownCtx P.Rational t ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => Known P.Rational t ('Just '( 'True, l)) ('Just '( 'True, r)) where
  type KnownCtx P.Rational t ('Just '( 'True, l)) ('Just '( 'True, r)) =
    (KR.KnownRational t, l <= t, t <= r)
  known' = UnsafeI . KR.rationalVal

instance forall t l r.
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  , KnownCtx P.Rational t ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => Known P.Rational t ('Just '( 'True, l)) ('Just '( 'False, r)) where
  type KnownCtx P.Rational t ('Just '( 'True, l)) ('Just '( 'False, r)) =
    (KR.KnownRational t, l <= t, t < r)
  known' = UnsafeI . KR.rationalVal

instance forall t l.
  ( Inhabited P.Rational ('Just '( 'True, l)) 'Nothing
  , KnownCtx P.Rational t ('Just '( 'True, l)) 'Nothing
  ) => Known P.Rational t ('Just '( 'True, l)) 'Nothing where
  type KnownCtx P.Rational t ('Just '( 'True, l)) 'Nothing =
    (KR.KnownRational t, l <= t)
  known' = UnsafeI . KR.rationalVal

instance forall t l r.
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  , KnownCtx P.Rational t ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => Known P.Rational t ('Just '( 'False, l)) ('Just '( 'True, r)) where
  type KnownCtx P.Rational t ('Just '( 'False, l)) ('Just '( 'True, r)) =
    (KR.KnownRational t, l < t, t <= r)
  known' = UnsafeI . KR.rationalVal

instance forall t l.
  ( Inhabited P.Rational ('Just '( 'False, l)) 'Nothing
  , KnownCtx P.Rational t ('Just '( 'False, l)) 'Nothing
  ) => Known P.Rational t ('Just '( 'False, l)) 'Nothing where
  type KnownCtx P.Rational t ('Just '( 'False, l)) 'Nothing =
    (KR.KnownRational t, l < t)
  known' = UnsafeI . KR.rationalVal

instance forall t r.
  ( Inhabited P.Rational 'Nothing ('Just '( 'True, r))
  , KnownCtx P.Rational t 'Nothing ('Just '( 'True, r))
  ) => Known P.Rational t 'Nothing ('Just '( 'True, r)) where
  type KnownCtx P.Rational t 'Nothing ('Just '( 'True, r)) =
    (KR.KnownRational t, t <= r)
  known' = UnsafeI . KR.rationalVal

instance forall t r.
  ( Inhabited P.Rational 'Nothing ('Just '( 'False, r))
  , KnownCtx P.Rational t 'Nothing ('Just '( 'False, r))
  ) => Known P.Rational t 'Nothing ('Just '( 'False, r)) where
  type KnownCtx P.Rational t 'Nothing ('Just '( 'False, r)) =
    (KR.KnownRational t, t < r)
  known' = UnsafeI . KR.rationalVal

instance forall t l r.
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  , KnownCtx P.Rational t ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => Known P.Rational t ('Just '( 'False, l)) ('Just '( 'False, r)) where
  type KnownCtx P.Rational t ('Just '( 'False, l)) ('Just '( 'False, r)) =
    (KR.KnownRational t, l < t, t < r)
  known' = UnsafeI . KR.rationalVal

instance forall t.
  ( KnownCtx P.Rational t 'Nothing 'Nothing
  ) => Known P.Rational t 'Nothing 'Nothing where
  type KnownCtx P.Rational t 'Nothing 'Nothing = KR.KnownRational t
  known' = UnsafeI . KR.rationalVal

--------------------------------------------------------------------------------

instance forall l r.
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  ) => With P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- le @l @t
      Dict <- le @t @r
      pure (g pt)

instance forall l r.
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  ) => With P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- le @l @t
      Dict <- lt @t @r
      pure (g pt)

instance forall l.
  ( Inhabited P.Rational ('Just '( 'True, l)) 'Nothing
  ) => With P.Rational ('Just '( 'True, l)) 'Nothing where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- le @l @t
      pure (g pt)

instance forall l.
  ( Inhabited P.Rational ('Just '( 'False, l)) 'Nothing
  ) => With P.Rational ('Just '( 'False, l)) 'Nothing where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- lt @l @t
      pure (g pt)

instance forall l r.
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  ) => With P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- lt @l @t
      Dict <- le @t @r
      pure (g pt)

instance forall r.
  ( Inhabited P.Rational 'Nothing ('Just '( 'True, r))
  ) => With P.Rational 'Nothing ('Just '( 'True, r)) where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- le @t @r
      pure (g pt)

instance forall l r.
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  ) => With P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- lt @l @t
      Dict <- lt @t @r
      pure (g pt)

instance forall r.
  ( Inhabited P.Rational 'Nothing ('Just '( 'False, r))
  ) => With P.Rational 'Nothing ('Just '( 'False, r)) where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    fromMaybe (error "I.with(Rational): impossible") $ do
      Dict <- lt @t @r
      pure (g pt)

instance With P.Rational 'Nothing 'Nothing where
  with x g | KR.SomeRational (pt :: Proxy t) <- KR.someRationalVal (unwrap x) =
    g pt

--------------------------------------------------------------------------------

instance (Inhabited P.Rational ('Just '(il, l)) 'Nothing, 0/1 <= l)
  => Plus P.Rational ('Just '(il, l)) 'Nothing where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

instance (Inhabited P.Rational 'Nothing ('Just '(ir, r)), r <= 0/1)
  => Plus P.Rational 'Nothing ('Just '(ir, r)) where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

instance Plus P.Rational 'Nothing 'Nothing where
  a `plus` b = UnsafeI (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance
  ( Inhabited P.Rational ('Just '(il, l)) 'Nothing, 1/1 <= l
  ) => Mult P.Rational ('Just '(il, l)) 'Nothing where
  a `mult` b = UnsafeI (unwrap a * unwrap b)

instance
  ( Inhabited P.Rational ('Just '(il, l)) ('Just '(ir, r)), 0/1 <= l, r <= 1/1
  ) => Mult P.Rational ('Just '(il, l)) ('Just '(ir, r)) where
  a `mult` b = UnsafeI (unwrap a * unwrap b)

instance Mult P.Rational 'Nothing 'Nothing where
  a `mult` b = UnsafeI (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance
  ( Inhabited P.Rational ('Just '(il, l)) ('Just '(ir, r)) , 0/1 < l, r <= 1/1
  ) => Div P.Rational ('Just '(il, l)) ('Just '(ir, r)) where
  a `div` b = UnsafeI (unwrap a / unwrap b)

--------------------------------------------------------------------------------

instance Minus P.Rational 'Nothing 'Nothing where
  a `minus` b = UnsafeI (unwrap a - unwrap b)

--------------------------------------------------------------------------------

instance
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  , l <= 0/1, 0/1 <= r
  ) => Zero P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  zero = UnsafeI 0

instance
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  , l <= 0/1, 0/1 < r
  ) => Zero P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  zero = UnsafeI 0

instance
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  , l < 0/1, 0/1 <= r
  ) => Zero P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  zero = UnsafeI 0

instance (Inhabited P.Rational ('Just '( 'True, l)) 'Nothing, l <= 0/1)
  => Zero P.Rational ('Just '( 'True, l)) 'Nothing where
  zero = UnsafeI 0

instance (Inhabited P.Rational ('Just '( 'False, l)) 'Nothing, l < 0/1)
  => Zero P.Rational ('Just '( 'False, l)) 'Nothing where
  zero = UnsafeI 0

instance (Inhabited P.Rational 'Nothing ('Just '( 'True, r)), 0/1 <= r)
  => Zero P.Rational 'Nothing ('Just '( 'True, r)) where
  zero = UnsafeI 0

instance (Inhabited P.Rational 'Nothing ('Just '( 'False, r)), 0/1 < r)
  => Zero P.Rational 'Nothing ('Just '( 'False, r)) where
  zero = UnsafeI 0

instance
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  , l < 0/1, 0/1 < r
  ) => Zero P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  zero = UnsafeI 0

instance Zero P.Rational 'Nothing 'Nothing where
  zero = UnsafeI 0

--------------------------------------------------------------------------------

instance
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  , l <= 1/1, 1/1 <= r
  ) => One P.Rational ('Just '( 'True, l)) ('Just '( 'True, r)) where
  one = UnsafeI 1

instance
  ( Inhabited P.Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  , l <= 1/1, 1/1 < r
  ) => One P.Rational ('Just '( 'True, l)) ('Just '( 'False, r)) where
  one = UnsafeI 1

instance
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  , l < 1/1, 1/1 <= r
  ) => One P.Rational ('Just '( 'False, l)) ('Just '( 'True, r)) where
  one = UnsafeI 1

instance (Inhabited P.Rational ('Just '( 'True, l)) 'Nothing, l <= 1/1)
  => One P.Rational ('Just '( 'True, l)) 'Nothing where
  one = UnsafeI 1

instance (Inhabited P.Rational ('Just '( 'False, l)) 'Nothing, l < 1/1)
  => One P.Rational ('Just '( 'False, l)) 'Nothing where
  one = UnsafeI 1

instance (Inhabited P.Rational 'Nothing ('Just '( 'True, r)), 1/1 <= r)
  => One P.Rational 'Nothing ('Just '( 'True, r)) where
  one = UnsafeI 1

instance (Inhabited P.Rational 'Nothing ('Just '( 'False, r)), 1/1 < r)
  => One P.Rational 'Nothing ('Just '( 'False, r)) where
  one = UnsafeI 1

instance
  ( Inhabited P.Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  , l < 1/1, 1/1 < r
  ) => One P.Rational ('Just '( 'False, l)) ('Just '( 'False, r)) where
  one = UnsafeI 1

instance One P.Rational 'Nothing 'Nothing where
  one = UnsafeI 1

--------------------------------------------------------------------------------

instance
  ( Zero P.Rational ('Just '(i, l)) ('Just '(i, r))
  , l KR.== KR.Negate r
  ) => Negate P.Rational ('Just '(i, l)) ('Just '(i, r)) where
  negate = UnsafeI . P.negate . unwrap

instance Negate P.Rational 'Nothing 'Nothing where
  negate = UnsafeI . P.negate . unwrap

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

