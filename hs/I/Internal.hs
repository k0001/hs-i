{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

#include <MachDeps.h>
#include <HsBaseConfig.h>

module I.Internal where

import Control.Monad
import Data.Bits
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
import KindInteger qualified as K
import Prelude hiding (min, max, div, pred, succ)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

leNatural
  :: forall a b
  .  (L.KnownNat a, L.KnownNat b)
  => Maybe (Dict (a L.<= b))
leNatural = case L.cmpNat (Proxy @a) (Proxy @b) of
  L.EQI -> Just $ unsafeCoerce (Dict @())
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.GTI -> Nothing

leInteger
  :: forall (a :: K.Integer) (b :: K.Integer)
  .  (K.KnownInteger a, K.KnownInteger b)
  => Maybe (Dict (a L.<= b))
leInteger = case K.cmpInteger (Proxy @a) (Proxy @b) of
  L.EQI -> Just $ unsafeCoerce (Dict @())
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.GTI -> Nothing

--------------------------------------------------------------------------------

type role I nominal nominal nominal
-- | A value of type @x@ known to be within the __i__nterval determined
-- by the left end @l@ and right end @r@.
newtype I (x :: Type) (l :: L x) (r :: R x) = UnsafeI x
  -- ^ For @'UnsafeI' x@ to be safe, @'Known' x t l r@ needs to be satisfied,
  -- with @t@ being the type-level representation of the value of type @x@.
  deriving newtype (Eq, Ord, Show)

-- | The kind of the __t__ype-level representation of @x@ in @'I' x l r@,
-- as it appears in @'Known' x t l r@.
type family T (x :: Type) :: k

-- | The kind of @__l__@ in @'I' x l r@.
type family L (x :: Type) :: k

-- | The kind of @__r__@ in @'I' x l r@.
type family R (x :: Type) :: k

-- | __Min__imu __l__eft bound for @x@. All the values of type @x@ are at
-- least as @'MinL' x@ states, as required by 'wrap'.
type family MinL (x :: Type) :: L x

-- | __Max__imum __r__ight bound for @x@.  All the values of type @x@ are at
-- most as @'MinR' x@ states, as required by 'wrap'.
type family MaxR (x :: Type) :: R x

-- | Type-level verison of @__'minBound'__ :: x@. If @x@ is unbounded on the
-- left end, then it's ok to leave @'MinT' x@ undefined.
-- If defined, it should mean the same 'MinL' does.
type family MinT (x :: Type) :: T x

-- | Type-level verison of @__'maxBound'__ :: x@. If @x@ is unbounded on the
-- right end, then it's ok to leave @'MaxT' x@ undefined.
-- If defined, it should mean the same 'MaxR' does.
type family MaxT (x :: Type) :: T x

-- | Minimum value of type @x@ contained in the interval @'I' x l r@, if any.
-- If @'I' x l r@ is unbounded on the right end, then it's ok to leave
-- @'MinBoundI' x l r@ undefined. If defined, it should mean the same @l@ does.
-- type family MinBoundI (x :: Type) (l :: L x) (r :: R x) :: T x

-- | Maximum value of type @x@ contained in the interval @'I' x l r@, if any.
-- If @'I' x l r@ is unbounded on the right end, then it's ok to leave
-- @'MaxBoundI' x l r@ undefined. If defined, it should mean the same @r@ does.
-- type family MaxBoundI (x :: Type) (l :: L x) (r :: R x) :: T x

-- | For @'I' x l r@ to be a valid interval type, @'Interval' x l r@ needs
-- to be satisfied.
--
-- __NB__: When defining 'Interval' instances, instead of mentioning any
-- necessary constraints in the instance context, mention them them in
-- 'IntervalCtx'. By doing so, when an instance of @'Interval' x l r@ is
-- satisfied, @'IntervalConstraint' x l r@ is satisfied as well.
class IntervalCtx x l r => Interval (x :: Type) (l :: L x) (r :: R x) where
  -- | Constraints to be satisfied for @'I' x l r@ to be a valid interval type.
  type IntervalCtx x l r :: Constraint
  type IntervalCtx x l r = ()
  -- | Minimum value of type @x@ contained in the interval @'I' x l r@, if any.
  -- If @'I' x l r@ is unbounded on the left end, then it's ok to leave
  -- @'MinBoundI' x l r@ undefined. If defined, it should mean the same as @l@.
  type MinBoundI x l r :: T x
  -- | Maximum value of type @x@ contained in the interval @'I' x l r@, if any.
  -- If @'I' x l r@ is unbounded on the right end, then it's ok to leave
  -- @'MaxBoundI' x l r@ undefined. If defined, it should mean the same as @r@.
  type MaxBoundI x l r :: T x
  -- | Wrap the @x@ value in the interval @'I' x l r@, if it fits.
  --
  -- * Consider using 'wrap' if the interval includes all values of type @x@.
  --
  -- * Consider using 'known' if you have type-level knowledge
  -- about the value of @x@.
  --
  -- [Identity law]
  --
  -- @
  -- forall (x :: 'Type').
  --   /such that/ 'isJust' ('from' x).
  --     'fmap' 'unwrap' ('from' x)  ==  'Just' x
  -- @
  from :: x -> Maybe (I x l r)

-- | 'Interval's that are not empty implement an 'Inhabited' instance.
--
-- __NB__: When defining 'Inhabited' instances, instead of mentioning any
-- necessary constraints in the instance context, mention them them in
-- 'InhabitedCtx'. By doing so, when an instance of @'Inhabited' x l r@ is
-- satisfied, @'InhabitedConstraint' x l r@ is satisfied as well.
class (Interval x l r, InhabitedCtx x l r)
  => Inhabited (x :: Type) (l :: L x) (r :: R x) where
  -- | Constraints to be satisfied for @'I' x l r@ to be a inhabited.
  type InhabitedCtx x l r :: Constraint
  type InhabitedCtx x l r = ()
  -- | Proof that there is at least one element in the @'I' x l r@ interval.
  --
  -- No guarantees are made about the value of 'inhabitant' other than the
  -- fact that it is known to inhabit the interval.
  inhabitant :: I x l r

-- | Wrap @x@ in @'I' x l r@, making sure that @x@ is within the interval
-- ends by clamping it to @'MinBoundI' x l r@ if less than @l@, or to
-- @'MaxBoundI' x l r@ if more than @r@.
clamp
  :: forall x l r
  .  ( Known x (MinBoundI x l r) l r
     , Known x (MaxBoundI x l r) l r
     , Ord x )
  => x
  -> I x l r
clamp x
  | x <= unwrap (min @x @l @r) = min
  | x >= unwrap (max @x @l @r) = max
  | otherwise                  = UnsafeI x

-- | Intervals supporting /addition/.
class (Inhabited x l r, PlusCtx x l r)
  => Plus (x :: Type) (l :: L x) (r :: R x) where
  type PlusCtx x l r :: Constraint
  type PlusCtx x l r = ()
  -- | @a `'plus'` b@ adds @a@ and @b@
  -- 'Nothing' if the result would be out of the interval.
  plus :: I x l r -> I x l r -> Maybe (I x l r)
  default plus :: (Bits x, Integral x)
               => I x l r -> I x l r -> Maybe (I x l r)
  a `plus` b = from =<< toIntegralSized (toInteger (unwrap a) +
                                         toInteger (unwrap b))

-- | Intervals supporting /multiplication/.
class (Inhabited x l r, MultCtx x l r)
  => Mult (x :: Type) (l :: L x) (r :: R x) where
  type MultCtx x l r :: Constraint
  type MultCtx x l r = ()
  -- | @a `'mult'` b@ multiples @a@ times @b@.
  -- 'Nothing' if the result would be out of the interval.
  mult :: I x l r -> I x l r -> Maybe (I x l r)
  default mult :: (Bits x, Integral x)
               => I x l r -> I x l r -> Maybe (I x l r)
  a `mult` b = from =<< toIntegralSized (toInteger (unwrap a) *
                                         toInteger (unwrap b))

-- | Intervals supporting /subtraction/.
class (Inhabited x l r, MinusCtx x l r)
  => Minus (x :: Type) (l :: L x) (r :: R x) where
  type MinusCtx x l r :: Constraint
  type MinusCtx x l r = ()
  -- | @a `'minus'` b@ substracts @b@ from @a@.
  -- 'Nothing' if the result would be out of the interval.
  minus :: I x l r -> I x l r -> Maybe (I x l r)
  default minus :: (Bits x, Integral x)
                => I x l r -> I x l r -> Maybe (I x l r)
  a `minus` b = from =<< toIntegralSized (toInteger (unwrap a) -
                                          toInteger (unwrap b))

-- | Intervals supporting /zero/.
class (Inhabited x l r, ZeroCtx x l r)
  => Zero (x :: Type) (l :: L x) (r :: R x) where
  type ZeroCtx x l r :: Constraint
  type ZeroCtx x l r = ()
  -- | Zero.
  zero :: I x l r

-- | Intervals supporting /one/.
class (Inhabited x l r, OneCtx x l r)
  => One (x :: Type) (l :: L x) (r :: R x) where
  type OneCtx x l r :: Constraint
  type OneCtx x l r = ()
  -- | One.
  one :: I x l r

-- | Intervals supporting /additive inverse/.
class (Plus x l r, Zero x l r, Minus x l r, PlusInvCtx x l r)
  => PlusInv (x :: Type) (l :: L x) (r :: R x) where
  type PlusInvCtx x l r :: Constraint
  type PlusInvCtx x l r = ()
  -- | Additive inverse, if it fits in the interval.
  plusinv :: I x l r -> Maybe (I x l r)
  default plusinv :: (Bits x, Integral x) => I x l r -> Maybe (I x l r)
  plusinv i = from =<< toIntegralSized (negate (toInteger (unwrap i)))

-- | Intervals supporting /multiplicative inverse/.
class (Mult x l r, One x l r, MultInvCtx x l r)
  => MultInv (x :: Type) (l :: L x) (r :: R x) where
  type MultInvCtx x l r :: Constraint
  type MultInvCtx x l r = ()
  -- | Multiplicative inverse, if it fits in the interval.
  multinv :: I x l r -> Maybe (I x l r)
  default multinv :: (Real x, Fractional x) => I x l r -> Maybe (I x l r)
  multinv = from . fromRational . recip . toRational . unwrap

-- | @a `'div'` b@ divides @a@ by @b@. 'Nothing' if the result doesn't fit in
-- the interval.
div :: forall x l r. MultInv x l r => I x l r -> I x l r -> Maybe (I x l r)
div a b = mult a =<< multinv b

-- | Obtain the single element in the @'I' x l r@ interval.
single
  :: forall x l r
  .  ( MinBoundI x l r ~ MaxBoundI x l r
     , Known x (MinBoundI x l r) l r )
  => I x l r
single = inhabitant
{-# inline single #-}

class
  ( Inhabited x l r, PredCtx x l r
  ) => Pred (x :: Type) (l :: L x) (r :: R x) where
  type PredCtx x l r :: Constraint
  type PredCtx x l r = ()
  -- | __Pred__ecessor. That is, the previous value within the interval.
  pred :: I x l r -> Maybe (I x l r)

class
  ( Inhabited x l r, SuccCtx x l r
  ) => Succ (x :: Type) (l :: L x) (r :: R x) where
  type SuccCtx x l r :: Constraint
  type SuccCtx x l r = ()
  -- | __Succ__essor. That is, the next value within the interval.
  succ :: I x l r -> Maybe (I x l r)

-- | Proof that @t@ is __known__ to be within @l@ and @r@ in @'I' x l r@.
--
-- __NB__: When defining 'Known' instances, instead of mentioning any
-- necessary constraints in the instance context, mention them them in
-- 'KnownCtx'. By doing so, when an instance of @'Known' x l r@ is
-- satisfied, @'KnownCtx' x l r@ is satisfied as well.
class
  ( Inhabited x l r, KnownCtx x t l r
  ) => Known (x :: Type) (t :: T x) (l :: L x) (r :: R x) where
  -- | Constraints to be satisfied by @t@ if it is known to be within
  -- the @'I' x l r@ interval.
  type KnownCtx x t l r :: Constraint
  type KnownCtx x t l r = ()
  -- | Obtain a term-level representation of @t@ as @'I' x l r@.
  --
  -- The type-parameters to 'known' are expected to be applied
  -- using @TypeApplications@ in @__x t l r__@ order:
  --
  -- @
  -- > :type 'known'
  -- /'known' :: forall (__x__ :: 'Type') (__t__ :: 'T' x) (__l__ :: 'L' x) (__r__ :: 'R' x). 'Known' x t l r => 'I' x l r/
  --
  -- > :type 'known' \@'Word8'
  -- /'known' \@'Word8' :: 'Known' 'Word8' t l r => 'I' 'Word8' l r/
  --
  -- > :type 'known' \@'Word8' \@55
  -- /'known' \@'Word8' \@55 :: 'Known' 'Word8' 55 l r => 'I' 'Word8' l r/
  --
  -- > :type 'known' \@'Word8' \@55 \@33
  -- /'known' \@'Word8' \@55 \@33 :: 'Known' 'Word8' 55 33 r => 'I' 'Word8' 33 r/
  --
  -- > :type 'known' \@'Word8' \@55 \@33 \@77
  -- /'known' \@'Word8' \@55 \@33 \@77 :: 'I' 'Word8' 33 77/
  --
  -- > 'known' \@'Word8' \@55 \@33 \@77
  -- /55/
  -- @
  --
  -- You may skip applying of the type-parameters using @__\@___@ if they can
  -- be inferred by other means. This is particularly handy if you can skip
  -- @x@. For example, in the following examples we are letting the
  -- type-checker know by other means that @x@ is expected to be 'Word8', so
  -- we can use the @\@_@ placeholder as first @TypeApplications@ parameter
  -- to 'known':
  --
  -- @
  -- > :type 'known' __\@___
  -- /'known' :: forall __{__x :: 'Type'__}__ (__t__ :: 'T' x) (__l__ :: 'L' x) (__r__ :: 'R' x). 'Known' x t l r => 'I' x l r/
  --
  -- > :type 'known' \@_ \@55 :: 'Known' 'Word8' 55 l r => 'I' 'Word8' l r
  -- /'known' \@_ \@55 :: 'Known' 'Word8' 55 l r => 'I' 'Word8' l r/
  --
  -- > :type 'known' \@_ \@55 \@33 :: 'Known' 'Word8' 55 33 r => 'I' 'Word8' 33 r
  -- /'known' \@_ \@55 \@33 :: 'Known' 'Word8' 55 33 r => 'I' 'Word8' 33 r/
  --
  -- > :type 'known' \@_ \@55 \@33 \@77 :: 'I' 'Word8' 33 77
  -- /'known' \@_ \@55 \@33 \@77 :: 'I' 'Word8' 33 77 :: 'I' 'Word8' 33 77/
  --
  -- > 'known' \@_ \@55 \@33 \@77 :: 'I' 'Word8' 33 77
  -- /55/
  -- @
  known :: I x l r

class (Known x t l r, Pred x l r, KnownPredCtx x t l r)
  => KnownPred (x :: Type) (t :: T x) (l :: L x) (r :: R x) where
  type KnownPredCtx x t l r :: Constraint
  type KnownPredCtx x t l r = ()
  -- | Type-level version of 'pred'.
  type Pred' x t l r :: T x

class (Known x t l r, Succ x l r, KnownSuccCtx x t l r)
  => KnownSucc (x :: Type) (t :: T x) (l :: L x) (r :: R x) where
  type KnownSuccCtx x t l r :: Constraint
  type KnownSuccCtx x t l r = ()
  -- | Type-level version of 'succ'.
  type Succ' x t l r :: T x

-- | Proof that @'I' x l r@ contains a value of type @x@ whose
-- type-level representation @t :: 'T' x@ satisfies a @'Known' x t l r@.
class (Inhabited x l r) => With (x :: Type) (l :: L x) (r :: R x) where
  -- | Bring to scope the type-level representation of @x@ as @t :: 'T' x@,
  -- together with the constraints that prove that @t@ is 'Known' to be in the
  -- interval @'I' x l r@.
  --
  -- [Identity law]
  --
  -- @
  -- x  ==  'with' x (\\(_ :: 'Proxy' t) -> 'known' \@_ \@t)
  -- @
  with :: I x l r -> (forall (t :: T x). Known x t l r => Proxy t -> b) -> b

-- | Wrap the given @x@ in the interval @'I' x ('MinL' x) ('MaxR' x)@.
--
-- This function always succeeds because such interval known to fit all the
-- values of type @x@.
--
-- * Consider using 'from' if the interval is not as big a @x@.
--
-- * Consider using 'known' if you have type-level knowledge
-- about the value of @x@.
--
-- [Identity law]
--
-- @
-- 'wrap' . 'unwrap' == 'id'
-- 'unwrap' . 'wrap' == 'id'
-- @
wrap :: Inhabited x (MinL x) (MaxR x) => x -> I x (MinL x) (MaxR x)
wrap = coerce
{-# INLINE wrap #-}

-- | Obtain the @x@ that is wrapped in the @'I' x l r@.
--
-- [Identity law]
--
-- @
-- 'wrap' . 'unwrap' == 'id'
-- 'unwrap' . 'wrap' == 'id'
-- @
--
-- It is implied that the interval is 'Inhabited', but there's no need
-- to mention the constraint here since otherwise it wouldn't have been
-- possible to obtain the input @'I' x l r@.
unwrap :: forall x l r. I x l r -> x
unwrap = coerce
{-# INLINE unwrap #-}

-- | Minimum value in the interval, if @'MinBoundI' x@ is defined.
min :: forall x l r. Known x (MinBoundI x l r) l r => I x l r
min = known @_ @(MinBoundI x l r)

-- | Maximum value in the interval, if @'MaxBoundI' x@ is defined.
max :: forall x l r. Known x (MaxBoundI x l r) l r => I x l r
max = known @_ @(MaxBoundI x l r)

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

type instance T Int8 = K.Integer
type instance L Int8 = K.Integer
type instance R Int8 = K.Integer
type instance MinT Int8 = K.N 128
type instance MaxT Int8 = K.P 127

type instance T Int16 = K.Integer
type instance L Int16 = K.Integer
type instance R Int16 = K.Integer
type instance MinT Int16 = K.N 32768
type instance MaxT Int16 = K.P 32767

type instance T Int32 = K.Integer
type instance L Int32 = K.Integer
type instance R Int32 = K.Integer
type instance MinT Int32 = K.N 2147483648
type instance MaxT Int32 = K.P 2147483647

type instance T Int64 = K.Integer
type instance L Int64 = K.Integer
type instance R Int64 = K.Integer
type instance MinT Int64 = K.N 9223372036854775808
type instance MaxT Int64 = K.P 9223372036854775807

type instance T Int = K.Integer
type instance L Int = K.Integer
type instance R Int = K.Integer
type instance MinT Int = K.N (L.Div (2 L.^ WORD_SIZE_IN_BITS) 2)
type instance MaxT Int = K.P (L.Div (2 L.^ WORD_SIZE_IN_BITS) 2 L.- 1)

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

