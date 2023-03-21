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
import GHC.TypeNats qualified as N
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
    ( N.KnownNat l
    , N.KnownNat r
    , MinT Natural <= l
    , l <= r )
  type MinI Natural l ('Just r) = l
  type MaxI Natural l ('Just r) = r

instance forall l.
  ( IntervalCtx Natural l 'Nothing
  ) => Interval Natural l 'Nothing where
  type IntervalCtx Natural l 'Nothing = (N.KnownNat l, MinT Natural <= l)
  type MinI Natural l 'Nothing = l

--------------------------------------------------------------------------------

instance
  ( Interval Natural l ('Just r), InhabitedCtx Natural l ('Just r)
  ) => Inhabited Natural l ('Just r) where
  type InhabitedCtx Natural l ('Just r) = ()
  inhabitant = min
  from = \x -> unsafest x <$ guard (l <= x && x <= r)
    where l = N.natVal (Proxy @l)
          r = N.natVal (Proxy @r)
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
  from = \x -> unsafest x <$ guard (l <= x)
    where l = N.natVal (Proxy @l)
  a `plus'` b = pure (a `plus` b)
  a `mult'` b = pure (a `mult` b)
  a `minus'` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                           toInteger (unwrap b))
  a `div'` b = do guard (unwrap b /= 0)
                  (q, 0) <- pure $ divMod (unwrap a) (unwrap b)
                  from q

--------------------------------------------------------------------------------

instance (Inhabited Natural l ('Just r)) => Clamp Natural l ('Just r)

instance (Inhabited Natural l 'Nothing) => Clamp Natural l 'Nothing where
  clamp = \case
    x | x <= unwrap min_ -> min_
      | otherwise -> unsafe x
    where min_ = min

--------------------------------------------------------------------------------

instance
  ( Inhabited Natural ld ('Just rd)
  , Inhabited Natural lu ('Just ru)
  , lu <= ld
  , rd <= ru)
  => Up Natural ld ('Just rd) lu ('Just ru)

instance
  ( Inhabited Natural ld yrd
  , Inhabited Natural lu 'Nothing
  , lu <= ld )
  => Up Natural ld yrd lu 'Nothing

--------------------------------------------------------------------------------

instance forall l r t.
  ( Inhabited Natural l ('Just r), KnownCtx Natural l ('Just r) t
  ) => Known Natural l ('Just r) t where
  type KnownCtx Natural l ('Just r) t = (N.KnownNat t, l <= t, t <= r)
  known' = unsafe . N.natVal

instance forall t l.
  ( Inhabited Natural l 'Nothing, KnownCtx Natural l 'Nothing t
  ) => Known Natural l 'Nothing t where
  type KnownCtx Natural l 'Nothing t = (N.KnownNat t, l <= t)
  known' = unsafe . N.natVal

--------------------------------------------------------------------------------

instance forall l r. (Inhabited Natural l ('Just r))
  => With Natural l ('Just r) where
  with x g = case N.someNatVal (unwrap x) of
    N.SomeNat (pt :: Proxy t) ->
      fromMaybe (error "I.with(Natural): impossible") $ do
        Dict <- leNatural @l @t
        Dict <- leNatural @t @r
        pure (g pt)

instance forall l. (Inhabited Natural l 'Nothing)
  => With Natural l 'Nothing where
  with x g = case N.someNatVal (unwrap x) of
    N.SomeNat (pt :: Proxy t) ->
      fromMaybe (error "I.with(Natural): impossible") $ do
        Dict <- leNatural @l @t
        pure (g pt)

--------------------------------------------------------------------------------

instance (Inhabited Natural l ('Just r), l /= r)
  => Discrete Natural l ('Just r) where
  pred' i = unsafe (unwrap i - 1) <$ guard (min < i)
  succ' i = unsafe (unwrap i + 1) <$ guard (i < max)

instance (Inhabited Natural l 'Nothing) => Discrete Natural l 'Nothing where
  pred' i = unsafe (unwrap i - 1) <$ guard (min < i)
  succ' = pure . succ

--------------------------------------------------------------------------------

instance (Inhabited Natural l 'Nothing) => Plus Natural l 'Nothing where
  plus a b = unsafe (unwrap a + unwrap b)

--------------------------------------------------------------------------------

instance (Inhabited Natural l 'Nothing) => Mult Natural l 'Nothing where
  mult a b = unsafe (unwrap a * unwrap b)

--------------------------------------------------------------------------------

instance (Discrete Natural l 'Nothing) => Succ Natural l 'Nothing where
  succ i = unsafe (unwrap i + 1)

--------------------------------------------------------------------------------

instance (Inhabited Natural 0 r) => Zero Natural 0 r where
  zero = unsafe 0

--------------------------------------------------------------------------------

instance (Inhabited Natural l 'Nothing, l <= 1) => One Natural l 'Nothing where
  one = unsafe 1

instance (Inhabited Natural l ('Just r), l <= 1, 1 <= r)
  => One Natural l ('Just r) where
  one = unsafe 1

--------------------------------------------------------------------------------

instance (Inhabited Natural l ('Just r)) => Shove Natural l ('Just r) where
  shove = \x -> unsafe $ mod x (r - l + 1) + l
    where l = unwrap (min @Natural @l @('Just r))
          r = unwrap (max @Natural @l @('Just r))

instance (Inhabited Natural l 'Nothing) => Shove Natural l 'Nothing where
   shove = \x -> unsafe $ if x < l then l + (l - x) else x
     where l = unwrap (min @Natural @l @'Nothing)

