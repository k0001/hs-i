{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module I.Test.Support
  ( genWord8
  , genInt8
  , genNatural
  , genInteger
  , genRational

  , genIWord8
  , genIInt8
  , genINatural
  , genIInteger
  , genIRational

  , leNatural
  , leInteger
  , leRational
  , ltRational

  , negateInteger
  , negateRational
  ) where

import Data.Constraint
import Data.Int
import Data.Proxy
import Data.Type.Ord
import Data.Word
import GHC.Real ((%))
import GHC.TypeLits qualified as L
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import KindInteger qualified as KI
import KindRational qualified as KR
import Numeric.Natural
import Unsafe.Coerce (unsafeCoerce)

import I (I)
import I qualified

--------------------------------------------------------------------------------

genWord8 :: MonadGen m => m Word8
genWord8 = Gen.integral $ Range.constant minBound maxBound

genInt8 :: MonadGen m => m Int8
genInt8 = Gen.integral $ Range.constant minBound maxBound

genNatural :: MonadGen m => m Natural
genNatural = Gen.integral $ Range.linear 0 (10 ^ (10 :: Int))

genInteger :: MonadGen m => m Integer
genInteger = Gen.integral $ Range.linearFrom 0 (negate (10 ^ (10 :: Int)))
                                               (10 ^ (10 :: Int))

genRational :: MonadGen m => m Rational
genRational = do
  n <- genInteger
  d <- Gen.integral $ Range.linear 1 (10 ^ (10 :: Int))
  pure (n % d)

--------------------------------------------------------------------------------

genIWord8 :: forall l r m. (MonadGen m, I.Shove Word8 l r) => m (I Word8 l r)
genIWord8 = I.shove <$> genWord8


genIInt8 :: forall l r m. (MonadGen m, I.Shove Int8 l r) => m (I Int8 l r)
genIInt8 = I.shove <$> genInt8

genINatural
  :: forall l r m. (MonadGen m, I.Shove Natural l r) => m (I Natural l r)
genINatural = I.shove <$> genNatural

genIInteger
  :: forall l r m. (MonadGen m, I.Shove Integer l r) => m (I Integer l r)
genIInteger = I.shove <$> genInteger

genIRational
  :: forall l r m. (MonadGen m, I.Shove Rational l r) => m (I Rational l r)
genIRational = I.shove <$> genRational

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
  => Maybe (Dict (a <= b))
leInteger = case KI.cmpInteger (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Just $ unsafeCoerce (Dict @())
  L.GTI -> Nothing

leRational
  :: forall (a :: KR.Rational) (b :: KR.Rational)
  .  (KR.KnownRational a, KR.KnownRational b)
  => Maybe (Dict (a <= b))
leRational = case KR.cmpRational (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Just $ unsafeCoerce (Dict @())
  L.GTI -> Nothing

ltRational
  :: forall (a :: KR.Rational) (b :: KR.Rational)
  .  (KR.KnownRational a, KR.KnownRational b)
  => Maybe (Dict (a < b))
ltRational = case KR.cmpRational (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Nothing
  L.GTI -> Nothing

--------------------------------------------------------------------------------
-- TODO: Move this stuff to KindInteger (ideas from Data.Constraint.Nat)

newtype IntegerMagic n =
  IntegerMagic (KI.KnownInteger n => Dict (KI.KnownInteger n))

-- WARNING: Causes SIGSEGV on GHCi 9.4.3. See GHC issue #19667. The workaround
-- is to run tests with `cabal test` instead.
integerMagic1
  :: forall a b
  .  (Integer -> Integer)  -- ^ a -> b
  -> (KI.KnownInteger a :- KI.KnownInteger b)
integerMagic1 f = Sub $ unsafeCoerce (IntegerMagic Dict)
                      $ f (KI.integerVal (Proxy @a))

negateInteger
  :: forall (a :: KI.Integer)
  .  (KI.KnownInteger a) :- (KI.KnownInteger (KI.Negate a))
negateInteger = integerMagic1 negate


--------------------------------------------------------------------------------
-- TODO: Move this stuff to KindRational (ideas from Data.Constraint.Nat)

newtype RationalMagic n =
  RationalMagic (KR.KnownRational n => Dict (KR.KnownRational n))

-- WARNING: Causes SIGSEGV on GHCi 9.4.3. See GHC issue #19667. The workaround
-- is to run tests with `cabal test` instead.
rationalMagic1
  :: forall a b
  .  (Rational -> Rational)  -- ^ a -> b
  -> (KR.KnownRational a :- KR.KnownRational b)
rationalMagic1 f = Sub $ unsafeCoerce (RationalMagic Dict)
                       $ f (KR.rationalVal (Proxy @a))

negateRational
  :: forall (a :: KR.Rational)
  .  (KR.KnownRational a) :- (KR.KnownRational (KR.Negate a))
negateRational = rationalMagic1 negate

