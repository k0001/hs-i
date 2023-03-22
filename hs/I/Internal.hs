{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

#include <MachDeps.h>
#include <HsBaseConfig.h>

module I.Internal where

import Data.Coerce
import Data.Constraint
import Data.Kind
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Word
import Data.Type.Equality
import Foreign.C.Types
import GHC.TypeLits qualified as L
import GHC.Stack
import KindInteger qualified as KI
import KindRational qualified as KR
import Prelude hiding (min, max, div, pred, succ, recip, negate)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

leNatural
  :: forall a b
  .  (L.KnownNat a, L.KnownNat b)
  => Maybe (Dict (a L.<= b))
leNatural = case L.cmpNat (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Just $ unsafeCoerce (Dict @())
  L.GTI -> Nothing

leInteger
  :: forall (a :: KI.Integer) (b :: KI.Integer)
  .  (KI.KnownInteger a, KI.KnownInteger b)
  => Maybe (Dict (a L.<= b))
leInteger = case KI.cmpInteger (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Just $ unsafeCoerce (Dict @())
  L.GTI -> Nothing

--------------------------------------------------------------------------------

type role I nominal nominal nominal
-- | A value of type @x@ known to be within the __i__nterval determined
-- by the left end @l@ and right end @r@.
newtype I (x :: Type) (l :: L x) (r :: R x) = UnsafeI x
  -- ^ For @'UnsafeI' x@ to be safe, @'Known' x l r t@ needs to be satisfied,
  -- with @t@ being the type-level representation of the value of type @x@.
  deriving newtype (Eq, Ord, Show)

-- | The kind of the __t__ype-level representation of @x@ in @'I' x l r@,
-- as it appears in @'Known' x l r t@.
type family T (x :: Type) :: k

-- | Type-level verison of @__'minBound'__ :: x@. If @x@ is unbounded on the
-- left end, then it's ok to leave @'MinT' x@ undefined.
-- If defined, it should match what 'MinL' means.
type family MinT (x :: Type) :: T x

-- | Type-level verison of @__'maxBound'__ :: x@. If @x@ is unbounded on the
-- right end, then it's ok to leave @'MaxT' x@ undefined.
-- If defined, it should match what 'MaxR' means.
type family MaxT (x :: Type) :: T x

-- | The kind of @__l__@ in @'I' x l r@.
type family L (x :: Type) :: k

-- | __Min__imum __l__eft bound for @x@. All the values of type @x@ are at
-- least as @'MinL' x@ says, as required by 'wrap'.
type family MinL (x :: Type) :: L x

-- | The kind of @__r__@ in @'I' x l r@.
type family R (x :: Type) :: k

-- | __Max__imum __r__ight bound for @x@.  All the values of type @x@ are at
-- most as @'MaxR' x@ says, as required by 'wrap'.
type family MaxR (x :: Type) :: R x


-- | For @'I' x l r@ to be a valid interval type, @'Interval' x l r@ needs
-- to be satisfied. All 'Interval's are non-empty.
--
-- __NB__: When defining 'Interval' instances, instead of mentioning any
-- necessary constraints in the instance context, mention them them in
-- 'IntervalCtx'. By doing so, when an instance of @'Interval' x l r@ is
-- satisfied, @'IntervalCtx' x l r@ is satisfied as well. If you don't do
-- this, 'with' won't behave as you would expect.
class IntervalCtx x l r => Interval (x :: Type) (l :: L x) (r :: R x) where
  -- | Constraints to be satisfied for @'I' x l r@ to be a valid non-empty
  -- interval type.
  type IntervalCtx x l r :: Constraint
  type IntervalCtx x l r = ()

  -- | Minimum value of type @x@ contained in the interval @'I' x l r@, if any.
  -- If @'I' x l r@ is unbounded on the left end, then it's ok to leave
  -- @'MinI' x l r@ undefined. If defined, it should mean the same as @l@.
  type MinI x l r :: T x
  type MinI x l r = L.TypeError
    ('L.Text "MinI not defined in instance ‘" 'L.:<>:
     'L.ShowType (Interval x l r) 'L.:<>: 'L.Text "’")

  -- | Maximum value of type @x@ contained in the interval @'I' x l r@, if any.
  -- If @'I' x l r@ is unbounded on the right end, then it's ok to leave
  -- @'MaxI' x l r@ undefined. If defined, it should mean the same as @r@.
  type MaxI x l r :: T x
  type MaxI x l r = L.TypeError
    ('L.Text "MaxI not defined in instance ‘" 'L.:<>:
     'L.ShowType (Interval x l r) 'L.:<>: 'L.Text "’")

  -- | Proof that there is at least one element in the @'I' x l r@ interval.
  --
  -- No guarantees are made about the value of 'inhabitant' other than the
  -- fact that it is known to inhabit the interval. The only exception to this
  -- are intervals that contain a single inhabitant, in which case
  -- 'inhabitant' will produce it. See 'single'.
  inhabitant :: I x l r

  -- | Wrap the @x@ value in the interval @'I' x l r@, if it fits.
  --
  -- * Consider using 'wrap' if the interval includes all values of type @x@.
  --
  -- * Consider using 'known' if you have type-level knowledge
  -- about the value of @x@.
  --
  -- * Consider using 'unsafe' if you know that the @x@ is within the interval.
  --
  -- [Identity law]
  --
  --     @
  --     forall (x :: 'Type').
  --       /such that/ 'isJust' ('from' x).
  --         'fmap' 'unwrap' ('from' x)  ==  'Just' x
  --     @
  from :: x -> Maybe (I x l r)

  -- | @'plus'' a b@ adds @a@ and @b@.
  --
  -- 'Nothing' if the result would be out of the interval. See 'plus', too.
  plus' :: I x l r -> I x l r -> Maybe (I x l r)
  plus' _ _ = Nothing

  -- | @'mult'' a b@ multiplies @a@ times @b@.
  --
  -- 'Nothing' if the result would be out of the interval. See 'mult', too.
  mult' :: I x l r -> I x l r -> Maybe (I x l r)
  mult' _ _ = Nothing

  -- | @'minus'' a b@ substracts @b@ from @a@.
  --
  -- 'Nothing' if the result would be out of the interval. See 'minus', too.
  minus' :: I x l r -> I x l r -> Maybe (I x l r)
  minus' a b = plus' a =<< negate' b
  {-# INLINE minus' #-}

  -- | @'negate'' a@ is the additive inverse of @a@.
  --
  -- 'Nothing' if the result would be out of the interval.  See 'negate', too.
  negate' :: I x l r -> Maybe (I x l r)
  negate' _ = Nothing

  -- | @'recip'' a@ is the multiplicative inverse of @a@.
  --
  -- 'Nothing' if the result would be out of the interval.
  recip' :: I x l r -> Maybe (I x l r)
  recip' _ = Nothing

  -- | @'div'' a b@ divides @a@ by @b@.
  --
  -- 'Nothing' if the result would be out of the interval. See 'div' too.
  div' :: I x l r -> I x l r -> Maybe (I x l r)
  div' a b = mult' a =<< recip' b
  {-# INLINE div' #-}

-- | 'unsafe' allows you to wrap an @x@ in an @'I' x l r@, failing
-- with 'error' if the @x@ is outside the interval.
--
-- __WARNING__: This function calls 'from', which means that you can't use
-- it to implement 'from'. You will have to use 'unsafest' in that case.
-- Your code will loop indefinitely otherwise.
unsafe :: forall x l r. (HasCallStack, Interval x l r) => x -> I x l r
unsafe = fromMaybe (error "I.unsafe: input outside interval") . from
{-# INLINE unsafe #-}

-- | 'unsafest' allows you to wrap an @x@ in an @'I' x l r@ without
-- checking whether the @x@ is within the interval ends.
--
-- __WARNING__: This function is fast because it doesn't do any work, but also
-- it is very dangerous because it ignores all the safety supposedly given by
-- the @'I' x l r@ type.  Don't use this unless you have proved by other means
-- that the @x@ is within the @'I' x l r@ interval.
-- Please use 'from' instead, or even 'unsafe'.
unsafest :: forall x l r. x -> I x l r
unsafest = coerce
{-# INLINE unsafest #-}

-- | 'Interval's that support clamping.
class (Interval x l r) => Clamp (x :: Type) (l :: L x) (r :: R x) where
  -- | Wrap @x@ in @'I' x l r@, making sure that @x@ is within the interval
  -- ends by clamping it to @'MinI' x l r@ if less than @l@, or to
  -- @'MaxI' x l r@ if more than @r@, if necessary.
  clamp :: x -> I x l r
  default clamp
    :: ( Known x l r (MinI x l r)
       , Known x l r (MaxI x l r)
       , Ord x )
    => x
    -> I x l r
  clamp = \case
    x | x <= unwrap min_ -> min_
      | x >= unwrap max_ -> max_
      | otherwise -> unsafe x
    where min_ = min -- for both type-inferrence and memoizing purposes
          max_ = max

-- | Downcast @'I' x lu ru@ into @'I' x ld rd@ if wrapped @x@ value fits
-- in @'I' x ld rd@.
down :: forall x lu ru ld rd
     .  (Interval x ld rd)
      => I x lu ru
      -> Maybe (I x ld rd)
down = from . unwrap
{-# INLINE down #-}

-- | 'Interval's that can be upcasted to a larger 'Interval' type.
class
  ( Interval x           ld          rd
  , Interval x                                   lu          ru
  ) => Up   (x :: Type) (ld :: L x) (rd :: R x) (lu :: L x) (ru :: R x)
  where
  -- | Proof that @'I' x ld rd@ can be upcasted into @'I' x lu ru@.
  --
  -- [Identity law]
  --
  --     @
  --     forall (a :: 'I' x ld rd).
  --       ('Up' x ld rd lu ru) =>
  --         'unwrap' a == 'unwrap' ('up' a :: 'I' x lu ru)
  --     @
  up :: I x ld rd -> I x lu ru
  -- For safety reasons, the default implementation is @'unsafe' . 'unwrap'@,
  -- which means upcasting is not free, as it involves a runtime check and a
  -- posibility of 'error'. Consider giving an implementation using 'unsafest'.
  default up :: HasCallStack => I x ld rd -> I x lu ru
  up = unsafe . unwrap
  {-# INLINE up #-}

-- | Identity. This instance is /INCOHERENT/, but that's OK because all
-- implementations of 'up' should give the same result, and this instance
-- is as fast as possible. So, it doesn't matter whether this instance
-- or another one is picked.
instance {-# INCOHERENT #-} (Interval x l r) => Up x l r l r where
  up = unsafest . unwrap
  {-# INLINE up #-}

-- | 'Interval's that contain /discrete/ elements.
class (Interval x l r) => Discrete (x :: Type) (l :: L x) (r :: R x) where
  -- | __Pred__ecessor. That is, the previous /discrete/ value in the interval.
  --
  -- 'Nothing' if the result would be out of the interval. See 'pred' too.
  pred' :: I x l r -> Maybe (I x l r)
  -- | __Succ__essor. That is, the next /discrete/ value in the interval.
  --
  -- 'Nothing' if the result would be out of the interval. See 'succ' too.
  succ' :: I x l r -> Maybe (I x l r)

-- | 'Interval's known to be inhabited by the number /zero/.
class (Interval x l r) => Zero (x :: Type) (l :: L x) (r :: R x) where
  -- | Zero.
  zero :: I x l r

-- | 'Interval's known to be inhabited by the number /one/.
class (Interval x l r) => One (x :: Type) (l :: L x) (r :: R x) where
  -- | One.
  one :: I x l r

-- | 'Interval's where /addition/ is known to be a closed operation.
class (Interval x l r) => Plus (x :: Type) (l :: L x) (r :: R x) where
  -- | @'plus' a b@ adds @a@ and @b@.
  --
  -- [Correspondence with 'plus'']
  --
  --     @
  --     forall (a :: 'I' x l r) (b :: 'I' x l r).
  --       ('Plus' x l r) =>
  --         'plus'' a b  ==  'Just' ('plus' a b)
  --     @
  plus :: I x l r -> I x l r -> I x l r

-- | 'Interval's where /multiplication/ is known to be a closed operation.
class (Interval x l r) => Mult (x :: Type) (l :: L x) (r :: R x) where
  -- | @'mult' a b@ multiplies @a@ times @b@.
  --
  -- [Correspondence with 'mult'']
  --
  --     @
  --     forall (a :: 'I' x l r) (b :: 'I' x l r).
  --       ('Mult' x l r) =>
  --         'mult'' a b  ==  'Just' ('mult' a b)
  --     @
  mult :: I x l r -> I x l r -> I x l r

-- | 'Interval's where /subtraction/ is known to be a closed operation.
class (Zero x l r) => Minus (x :: Type) (l :: L x) (r :: R x) where
  -- | @'minus' a b@ substracts @b@ from @a@
  --
  -- [Correspondence with 'minus'']
  --
  --     @
  --     forall (a :: 'I' x l r) (b :: 'I' x l r).
  --       ('Minus' x l r) =>
  --         'minus'' a b  ==  'Just' ('minus' a b)
  --     @
  minus :: I x l r -> I x l r -> I x l r

-- | 'Interval's where /negation/ is known to be a closed operation.
class (Zero x l r) => Negate (x :: Type) (l :: L x) (r :: R x) where
  -- | Additive inverse, if it fits in the interval.
  --
  -- [Identity law]
  --
  --     @
  --     forall (a :: 'I' x l r).
  --       ('Negate' x l r) =>
  --         a == 'negate' ('negate' a)
  --     @
  --
  -- [Correspondence with 'negate'']
  --
  --     @
  --     forall (a :: 'I' x l r) (b :: 'I' x l r).
  --       ('Minus' x l r) =>
  --         'negate'' a b  ==  'Just' ('negate' a b)
  --     @
  negate :: I x l r -> I x l r

-- | 'Discrete' 'Interval's where obtaining the /predecessor/ is knonwn
-- to be a closed operation.
class (Discrete x l r) => Pred (x :: Type) (l :: L x) (r :: R x) where
  -- | @'pred' a@ is the previous discrete value in the interval,
  -- the /predecessor/.
  --
  -- [Correspondence with 'pred'']
  --
  --     @
  --     forall (a :: 'I' x l r).
  --       ('Pred' x l r) =>
  --         'pred'' a  ==  'Just' ('pred' a)
  --     @
  pred :: I x l r -> I x l r

-- | 'Discrete' 'Interval's where obtaining the /successor/ is knonwn
-- to be a closed operation.
class (Discrete x l r) => Succ (x :: Type) (l :: L x) (r :: R x) where
  -- | @'succ' a@ is the next discrete value in the interval, the /successor/.
  --
  -- [Correspondence with 'succ'']
  --
  --     @
  --     forall (a :: 'I' x l r).
  --       ('Succ' x l r) =>
  --         'succ'' a  ==  'Just' ('succ' a)
  --     @
  succ :: I x l r -> I x l r

-- | 'Interval's where /division/ is known to be a closed operation.
class (Interval x l r) => Div (x :: Type) (l :: L x) (r :: R x) where
  -- | @'div' a b@ divides @a@ by @b@.
  --
  -- [Correspondence with 'div'']
  --
  --     @
  --     forall (a :: 'I' x l r) (b :: 'I' x l r).
  --       ('Div' x l r) =>
  --         'div'' a b  ==  'Just' ('div' a b)
  --     @
  div :: I x l r -> I x l r -> I x l r


-- | If an 'Interval' contains a /single/ 'inhabitant', obtain it.
single
  :: forall x l r
  .  ( MinI x l r ~ MaxI x l r
     , Known x l r (MinI x l r) )
  => I x l r
single = inhabitant
{-# INLINE single #-}

-- | Proof that @t@ is __known__ to be within @l@ and @r@ in @'I' x l r@.
--
-- __NB__: When defining 'Known' instances, instead of mentioning any
-- necessary constraints in the instance context, mention them them in
-- 'KnownCtx'. By doing so, when an instance of @'Known' x l r@ is
-- satisfied, @'KnownCtx' x l r@ is satisfied as well.  If you don't do
-- this, 'with' won't behave as you would expect.
class
  ( Interval x l r, KnownCtx x l r t
  ) => Known (x :: Type) (l :: L x) (r :: R x) (t :: T x) where
  -- | Constraints to be satisfied by @t@ if it is known to be within
  -- the @'I' x l r@ interval.
  type KnownCtx x l r t :: Constraint
  type KnownCtx x l r t = ()
  -- | Obtain a term-level representation of @t@ as @'I' x l r@.
  --
  -- Also consider using 'known', an alternative version of this function
  -- designed to be used with @-XTypeApplications@.
  known' :: Proxy t -> I x l r

-- | Alternative version of 'known'', designed to be used with
-- @-XTypeApplications@. It works only when @x@ can be inferred by other means.
--
-- @
-- > :type 'known'
-- /'known' :: forall __{__x :: 'Type'__}__ (__t__ :: 'T' x) (__l__ :: 'L' x) (__r__ :: 'R' x). 'Known' x l r t => 'I' x l r/
--
-- > :type 'known' \@55 :: 'Known' 'Word8' l r 55 => 'I' 'Word8' l r
-- /'known' \@55 :: 'Known' 'Word8' l r 55 => 'I' 'Word8' l r/
--
-- > :type 'known' \@55 \@33 :: 'Known' 'Word8' 33 r 55 => 'I' 'Word8' 33 r
-- /'known' \@55 \@33 :: 'Known' 'Word8' 33 r 55 => 'I' 'Word8' 33 r/
--
-- > :type 'known' \@55 \@33 \@77 :: 'I' 'Word8' 33 77
-- /'known' \@55 \@33 \@77 :: 'I' 'Word8' 33 77 :: 'I' 'Word8' 33 77/
--
-- > 'known' \@55 \@33 \@77 :: 'I' 'Word8' 33 77
-- /55/
-- @
known :: forall {x} t l r. Known x l r t => I x l r
known = known' (Proxy @t)
{-# INLINE known #-}

-- | Proof that @'I' x l r@ contains a value of type @x@ whose
-- type-level representation @t :: 'T' x@ satisfies a @'Known' x l r t@.

-- TODO: The 'with' method belongs in the 'Interval' class, but I can't
-- get it to type-check, so it's here in this separate 'With' class.
class (Interval x l r) => With (x :: Type) (l :: L x) (r :: R x) where
  -- | Bring to scope the type-level representation of @x@ as @t :: 'T' x@,
  -- together with the constraints that prove that @t@ is 'Known' to be in the
  -- interval @'I' x l r@.
  --
  -- [Identity law]
  --
  --     @
  --     x  ==  'with' x 'known''
  --     @
  with :: I x l r -> (forall (t :: T x). Known x l r t => Proxy t -> b) -> b

-- | Wrap the given @x@ in the interval @'I' x ('MinL' x) ('MaxR' x)@.
--
-- This function always succeeds because the interval known to fit all the
-- values of type @x@.
--
-- [Identity law]
--
--     @
--     'wrap' . 'unwrap' == 'id'
--     'unwrap' . 'wrap' == 'id'
--     @
--
-- If the interval is not as big as @x@:
--
--     * Consider using 'from'.
--
--     * Consider using 'known' if you have type-level knowledge
--     about the value of @x@.
--
--     * Consider using 'unsafe' if you know that the @x@ is within
--     the interval.
wrap :: Interval x (MinL x) (MaxR x) => x -> I x (MinL x) (MaxR x)
wrap = coerce
{-# INLINE wrap #-}

-- | Obtain the @x@ that is wrapped in the @'I' x l r@.
--
-- [Identity law]
--
--     @
--     'wrap' . 'unwrap' == 'id'
--     'unwrap' . 'wrap' == 'id'
--     @
unwrap :: forall x l r. I x l r -> x
unwrap = coerce
{-# INLINE unwrap #-}

--------------------------------------------------------------------------------

-- | Minimum value in the interval, if @'MinI' x@ is defined.
min :: forall x l r. Known x l r (MinI x l r) => I x l r
min = known @(MinI x l r)

-- | Maximum value in the interval, if @'MaxI' x@ is defined.
max :: forall x l r. Known x l r (MaxI x l r) => I x l r
max = known @(MaxI x l r)

instance
  ( Known x l r (MinI x l r)
  , Known x l r (MaxI x l r)
  ) => Bounded (I x l r) where
  minBound = min
  maxBound = max

--------------------------------------------------------------------------------

-- | Shove an @x@ into an interval @'I' x l r@, somehow.
--
-- Note: This class is for testing purposes only. For example, if you want to
-- generate random values of type @'I' x l r@ for testing purposes, all you
-- have to do is generate random values of type @x@ and then 'shove' them into
-- @'I' x l r@.
--
-- Note: We don't like this too much. If there was a good way to export
-- generators for Hedgehog or QuickCheck without depending on these libraries,
-- we'd probably export that instead.
class Interval x l r => Shove (x :: Type) (l :: L x) (r :: R x) where
  -- | No guarantees are made about the @x@ value that ends up in @'I' x l r@.
  -- In particular, you can't expect @'id' == 'unwrap' . 'shove'@, not even
  -- for @x@ values for which @'from' == 'Just'@. All 'shove' guarantees is
  -- a more or less uniform distribution.
  shove :: x -> I x l r

--------------------------------------------------------------------------------


{-
TODO: I have no idea why, but if I move the T, L or R type instance
definitions to the I.Naturals module, it does not compile.
It leads to errors like the following all over the module.

         hs/I/Naturals.hs:108:41: error:
             • Expected kind ‘Natural’, but ‘r’ has kind ‘R Word8’
             • In the second argument of ‘(<=)’, namely ‘r’
               In the type ‘(l <= 1, 1 <= r)’
               In the type instance declaration for ‘OneCtx’
             |
         108 |   type OneCtx Word8 l r = (l <= 1, 1 <= r)


On the contrary, it doesn't seem necessary to define define MinT and MaxT
instances here. However, it's convenient, so we do it.
It's easier to only deal with the CPP stuff here, too.
-}

type instance T Word8 = L.Natural
type instance L Word8 = L.Natural
type instance R Word8 = L.Natural
type instance MinT Word8 = 0
type instance MaxT Word8 = 255

type instance T Word16 = L.Natural
type instance L Word16 = L.Natural
type instance R Word16 = L.Natural
type instance MinT Word16 = 0
type instance MaxT Word16 = 65535

type instance T Word32 = L.Natural
type instance L Word32 = L.Natural
type instance R Word32 = L.Natural
type instance MinT Word32 = 0
type instance MaxT Word32 = 4294967295

type instance T Word64 = L.Natural
type instance L Word64 = L.Natural
type instance R Word64 = L.Natural
type instance MinT Word64 = 0
type instance MaxT Word64 = 18446744073709551615

type instance T Word = L.Natural
type instance L Word = L.Natural
type instance R Word = L.Natural
type instance MinT Word = 0
type instance MaxT Word = (2 L.^ WORD_SIZE_IN_BITS) L.- 1

type instance T Int8 = KI.Integer
type instance L Int8 = KI.Integer
type instance R Int8 = KI.Integer
type instance MinT Int8 = KI.N 128
type instance MaxT Int8 = KI.P 127

type instance T Int16 = KI.Integer
type instance L Int16 = KI.Integer
type instance R Int16 = KI.Integer
type instance MinT Int16 = KI.N 32768
type instance MaxT Int16 = KI.P 32767

type instance T Int32 = KI.Integer
type instance L Int32 = KI.Integer
type instance R Int32 = KI.Integer
type instance MinT Int32 = KI.N 2147483648
type instance MaxT Int32 = KI.P 2147483647

type instance T Int64 = KI.Integer
type instance L Int64 = KI.Integer
type instance R Int64 = KI.Integer
type instance MinT Int64 = KI.N 9223372036854775808
type instance MaxT Int64 = KI.P 9223372036854775807

type instance T Int = KI.Integer
type instance L Int = KI.Integer
type instance R Int = KI.Integer
type instance MinT Int = KI.N (L.Div (2 L.^ WORD_SIZE_IN_BITS) 2)
type instance MaxT Int = KI.P (L.Div (2 L.^ WORD_SIZE_IN_BITS) 2 L.- 1)

type instance T CChar = T HTYPE_CHAR
type instance L CChar = L HTYPE_CHAR
type instance R CChar = R HTYPE_CHAR
type instance MinT CChar = MinT HTYPE_CHAR
type instance MaxT CChar = MaxT HTYPE_CHAR

type instance T CSize = T HTYPE_SIZE_T
type instance L CSize = L HTYPE_SIZE_T
type instance R CSize = R HTYPE_SIZE_T
type instance MinT CSize = MinT HTYPE_SIZE_T
type instance MaxT CSize = MaxT HTYPE_SIZE_T

type instance T CClock = T HTYPE_CLOCK_T
type instance L CClock = L HTYPE_CLOCK_T
type instance R CClock = R HTYPE_CLOCK_T
type instance MinT CClock = MinT HTYPE_CLOCK_T
type instance MaxT CClock = MaxT HTYPE_CLOCK_T

type instance T CInt = T HTYPE_INT
type instance L CInt = L HTYPE_INT
type instance R CInt = R HTYPE_INT
type instance MinT CInt = MinT HTYPE_INT
type instance MaxT CInt = MaxT HTYPE_INT

type instance T CIntMax = T HTYPE_INTMAX_T
type instance L CIntMax = L HTYPE_INTMAX_T
type instance R CIntMax = R HTYPE_INTMAX_T
type instance MinT CIntMax = MinT HTYPE_INTMAX_T
type instance MaxT CIntMax = MaxT HTYPE_INTMAX_T

type instance T CIntPtr = T HTYPE_INTPTR_T
type instance L CIntPtr = L HTYPE_INTPTR_T
type instance R CIntPtr = R HTYPE_INTPTR_T
type instance MinT CIntPtr = MinT HTYPE_INTPTR_T
type instance MaxT CIntPtr = MaxT HTYPE_INTPTR_T

type instance T CLLong = T HTYPE_LONG_LONG
type instance L CLLong = L HTYPE_LONG_LONG
type instance R CLLong = R HTYPE_LONG_LONG
type instance MinT CLLong = MinT HTYPE_LONG_LONG
type instance MaxT CLLong = MaxT HTYPE_LONG_LONG

type instance T CLong = T HTYPE_LONG
type instance L CLong = L HTYPE_LONG
type instance R CLong = R HTYPE_LONG
type instance MinT CLong = MinT HTYPE_LONG
type instance MaxT CLong = MaxT HTYPE_LONG

type instance T CPtrdiff = T HTYPE_PTRDIFF_T
type instance L CPtrdiff = L HTYPE_PTRDIFF_T
type instance R CPtrdiff = R HTYPE_PTRDIFF_T
type instance MinT CPtrdiff = MinT HTYPE_PTRDIFF_T
type instance MaxT CPtrdiff = MaxT HTYPE_PTRDIFF_T

type instance T CSChar = T HTYPE_SIGNED_CHAR
type instance L CSChar = L HTYPE_SIGNED_CHAR
type instance R CSChar = R HTYPE_SIGNED_CHAR
type instance MinT CSChar = MinT HTYPE_SIGNED_CHAR
type instance MaxT CSChar = MaxT HTYPE_SIGNED_CHAR

type instance T CSUSeconds = T HTYPE_SUSECONDS_T
type instance L CSUSeconds = L HTYPE_SUSECONDS_T
type instance R CSUSeconds = R HTYPE_SUSECONDS_T
type instance MinT CSUSeconds = MinT HTYPE_SUSECONDS_T
type instance MaxT CSUSeconds = MaxT HTYPE_SUSECONDS_T

type instance T CShort = T HTYPE_SHORT
type instance L CShort = L HTYPE_SHORT
type instance R CShort = R HTYPE_SHORT
type instance MinT CShort = MinT HTYPE_SHORT
type instance MaxT CShort = MaxT HTYPE_SHORT

type instance T CTime = T HTYPE_TIME_T
type instance L CTime = L HTYPE_TIME_T
type instance R CTime = R HTYPE_TIME_T
type instance MinT CTime = MinT HTYPE_TIME_T
type instance MaxT CTime = MaxT HTYPE_TIME_T

type instance T CUChar = T HTYPE_UNSIGNED_CHAR
type instance L CUChar = L HTYPE_UNSIGNED_CHAR
type instance R CUChar = R HTYPE_UNSIGNED_CHAR
type instance MinT CUChar = MinT HTYPE_UNSIGNED_CHAR
type instance MaxT CUChar = MaxT HTYPE_UNSIGNED_CHAR

type instance T CUInt = T HTYPE_UNSIGNED_INT
type instance L CUInt = L HTYPE_UNSIGNED_INT
type instance R CUInt = R HTYPE_UNSIGNED_INT
type instance MinT CUInt = MinT HTYPE_UNSIGNED_INT
type instance MaxT CUInt = MaxT HTYPE_UNSIGNED_INT

type instance T CUIntMax = T HTYPE_UINTMAX_T
type instance L CUIntMax = L HTYPE_UINTMAX_T
type instance R CUIntMax = R HTYPE_UINTMAX_T
type instance MinT CUIntMax = MinT HTYPE_UINTMAX_T
type instance MaxT CUIntMax = MaxT HTYPE_UINTMAX_T

type instance T CUIntPtr = T HTYPE_UINTPTR_T
type instance L CUIntPtr = L HTYPE_UINTPTR_T
type instance R CUIntPtr = R HTYPE_UINTPTR_T
type instance MinT CUIntPtr = MinT HTYPE_UINTPTR_T
type instance MaxT CUIntPtr = MaxT HTYPE_UINTPTR_T

type instance T CULLong = T HTYPE_UNSIGNED_LONG_LONG
type instance L CULLong = L HTYPE_UNSIGNED_LONG_LONG
type instance R CULLong = R HTYPE_UNSIGNED_LONG_LONG
type instance MinT CULLong = MinT HTYPE_UNSIGNED_LONG_LONG
type instance MaxT CULLong = MaxT HTYPE_UNSIGNED_LONG_LONG

type instance T CULong = T HTYPE_UNSIGNED_LONG
type instance L CULong = L HTYPE_UNSIGNED_LONG
type instance R CULong = R HTYPE_UNSIGNED_LONG
type instance MinT CULong = MinT HTYPE_UNSIGNED_LONG
type instance MaxT CULong = MaxT HTYPE_UNSIGNED_LONG

type instance T CUSeconds = T HTYPE_USECONDS_T
type instance L CUSeconds = L HTYPE_USECONDS_T
type instance R CUSeconds = R HTYPE_USECONDS_T
type instance MinT CUSeconds = MinT HTYPE_USECONDS_T
type instance MaxT CUSeconds = MaxT HTYPE_USECONDS_T

type instance T CUShort = T HTYPE_UNSIGNED_SHORT
type instance L CUShort = L HTYPE_UNSIGNED_SHORT
type instance R CUShort = R HTYPE_UNSIGNED_SHORT
type instance MinT CUShort = MinT HTYPE_UNSIGNED_SHORT
type instance MaxT CUShort = MaxT HTYPE_UNSIGNED_SHORT

type instance T CWchar = T HTYPE_WCHAR_T
type instance L CWchar = L HTYPE_WCHAR_T
type instance R CWchar = R HTYPE_WCHAR_T
type instance MinT CWchar = MinT HTYPE_WCHAR_T
type instance MaxT CWchar = MaxT HTYPE_WCHAR_T

type instance T L.Natural = L.Natural
type instance L L.Natural = L.Natural
-- | ''Nothing' means /unbounded/.
type instance R L.Natural = Maybe L.Natural
type instance MinT L.Natural = 0

type instance T Integer = KI.Integer
-- | * ''Nothing' means /unbounded/.
--
-- * @''Just' t@ means up to @t@, /inclusive/.
type instance L Integer = Maybe KI.Integer
-- | * ''Nothing' means /unbounded/.
--
-- * @''Just' t@ means /up to @t@, inclusive/.
type instance R Integer = Maybe KI.Integer

type instance T Rational = KR.Rational
-- | * ''Nothing' means /unbounded/.
--
--  * @''Just (''True', t)@ means /up to @t@, inclusive/.
--
--  * @''Just (''False', t)@ means /up to @t@, exclusive/.
type instance L Rational = Maybe (Bool, KR.Rational)
-- | * ''Nothing' means /unbounded/.
--
--  * @''Just (''True', t)@ means /up to @t@, inclusive/.
--
--  * @''Just (''False', t)@ means /up to @t@, exclusive/.
type instance R Rational = Maybe (Bool, KR.Rational)

