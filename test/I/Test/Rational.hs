{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-imports #-}

module I.Test.Rational (tt) where

import Control.Monad
import Data.Constraint
import Data.Proxy
import Data.Type.Ord
import Hedgehog (failure, forAll, property, assert, (===), (/==))
import Hedgehog.Gen qualified as Gen
import KindInteger (N)
import KindRational (type (/))
import KindRational qualified as KR
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testProperty)

import I (I)
import I qualified

import I.Test.Support

--------------------------------------------------------------------------------

tt :: TestTree
tt = testGroup "Rational"
  [ testProperty "wrap" $ property $ do
      x <- forAll genRational
      x === I.unwrap (I.wrap x)

  , tt'cc @(N 100/1) @(N 100/1)
  , tt'cc @(N 100/1) @(N 10/1)
  , tt'cc @(N 100/1) @(N 1/1)
  , tt'cc @(N 100/1) @(N 0/1)
  , tt'cc @(N 1/1)   @(N 1/1)
  , tt'cc @(N 1/1)   @(0/1)
  , tt'cc @(0/1)     @(0/1)
  , tt'cc @(0/1)     @(1/1)
  , tt'cc @(1/1)     @(1/1)
  , tt'cc @(0/1)     @(100/1)
  , tt'cc @(1/1)     @(100/1)
  , tt'cc @(10/1)    @(100/1)
  , tt'cc @(100/1)   @(100/1)

  , tt'co @(N 100/1) @(N 10/1)
  , tt'co @(N 100/1) @(N 1/1)
  , tt'co @(N 100/1) @(N 0/1)
  , tt'co @(N 1/1)   @(0/1)
  , tt'co @(0/1)     @(1/1)
  , tt'co @(0/1)     @(100/1)
  , tt'co @(1/1)     @(100/1)
  , tt'co @(10/1)    @(100/1)

  , tt'cu @(N 100/1)
  , tt'cu @(N 1/1)
  , tt'cu @(0/1)
  , tt'cu @(1/1)
  , tt'cu @(100/1)

  , tt'oc @(N 100/1) @(N 10/1)
  , tt'oc @(N 100/1) @(N 1/1)
  , tt'oc @(N 100/1) @(N 0/1)
  , tt'oc @(N 1/1)   @(0/1)
  , tt'oc @(0/1)     @(1/1)
  , tt'oc @(0/1)     @(100/1)
  , tt'oc @(1/1)     @(100/1)
  , tt'oc @(10/1)    @(100/1)

  , tt'oo @(N 100/1) @(N 10/1)
  , tt'oo @(N 100/1) @(N 1/1)
  , tt'oo @(N 100/1) @(N 0/1)
  , tt'oo @(N 1/1)   @(0/1)
  , tt'oo @(0/1)     @(1/1)
  , tt'oo @(0/1)     @(100/1)
  , tt'oo @(1/1)     @(100/1)
  , tt'oo @(10/1)    @(100/1)

  , tt'ou @(N 100/1)
  , tt'ou @(N 1/1)
  , tt'ou @(0/1)
  , tt'ou @(1/1)
  , tt'ou @(100/1)

  , tt'uc @(N 100/1)
  , tt'uc @(N 1/1)
  , tt'uc @(0/1)
  , tt'uc @(1/1)
  , tt'uc @(100/1)

  , tt'uo @(N 100/1)
  , tt'uo @(N 1/1)
  , tt'uo @(0/1)
  , tt'uo @(1/1)
  , tt'uo @(100/1)

  , tt'uu
  ]

tt'cc
  :: forall (l :: KR.Rational) (r :: KR.Rational)
  .  I.Interval Rational ('Just '( 'True, l)) ('Just '( 'True, r))
  => TestTree
tt'cc = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @('Just '( 'True, l)) @('Just '( 'True, r)) x of
        Nothing -> assert (x < l' || x > r')
        Just y -> do assert (x >= l' && x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @('Just '( 'True, l)) @('Just '( 'True, r)) x
      I.from (I.unwrap y) === Just y
      if x < l' || x > r'
         then I.from @Rational @('Just '( 'True, l)) @('Just '( 'True, r)) x === Nothing
         else I.from @Rational @('Just '( 'True, l)) @('Just '( 'True, r)) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      b <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x < l' || x > r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      b <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x < l' || x > r')
        Just y -> I.unwrap y === x

  , case (leRational @(0/1) @l, leRational @r @(1/1)) of
      (Just Dict, Just Dict) ->
        pure $ testProperty "mult" $ property $ do
          a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
          b <- forAll $ genIRational
          Just (I.mult a b) === I.mult' a b
      _ -> mzero

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      b <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x < l' || x > r')
        Just y -> I.unwrap y === x

  , if (l' == 0 && r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , case (ltRational @(0/1) @l, leRational @r @(1/1)) of
      (Just Dict, Just Dict) -> pure $ testProperty "div" $ property $ do
        a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
        b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                                 (genIRational @('Just '( 'True, l))
                                               @('Just '( 'True, r)))
        I.div' a b === Just (I.div a b)
      _ -> mzero

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genRational
      case I.clamp @Rational @('Just '( 'True, l)) @('Just '( 'True, r)) x of
        y | x < l' -> I.unwrap y === l'
          | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      x === I.with x I.known'

  , case (leRational @l @(0/1), leRational @(0/1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Rational @('Just '( 'True, l))
                                         @('Just '( 'True, r)))
      _ -> mzero

  , case (leRational @l @(1/1), leRational @(1/1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @('Just '( 'True, l))
                                        @('Just '( 'True, r)))
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      case I.negate' x of
        Just y -> Just x === I.negate' y
        Nothing -> Nothing === I.from @Rational
                                      @('Just '( 'True, l))
                                      @('Just '( 'True, r))
                                      (negate (I.unwrap x))

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'True, r))
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))

  ]
  where
    l   = I.min         :: I Rational ('Just '( 'True, l)) ('Just '( 'True, r))
    l'  = I.unwrap l    :: Rational
    r   = I.max         :: I Rational ('Just '( 'True, l)) ('Just '( 'True, r))
    r'  = I.unwrap r    :: Rational

tt'co
  :: forall (l :: KR.Rational) (r :: KR.Rational)
  .  I.Interval Rational ('Just '( 'True, l)) ('Just '( 'False, r))
  => TestTree
tt'co = testGroup ("Interval [" <> show l <> ", " <> show r' <> ")")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @('Just '( 'True, l)) @('Just '( 'False, r)) x of
        Nothing -> assert (x < l' || x >= r')
        Just y -> do assert (x >= l' && x < r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @('Just '( 'True, l)) @('Just '( 'False, r)) x
      I.from (I.unwrap y) === Just y
      if x < l' || x >= r'
         then I.from @Rational @('Just '( 'True, l)) @('Just '( 'False, r)) x === Nothing
         else I.from @Rational @('Just '( 'True, l)) @('Just '( 'False, r)) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      b <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x < l' || x >= r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      b <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x < l' || x >= r')
        Just y -> I.unwrap y === x


  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      b <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x < l' || x >= r')
        Just y -> I.unwrap y === x

  , if (l' == 0 && r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , case (ltRational @(0/1) @l, leRational @r @(1/1)) of
      (Just Dict, Just Dict) -> pure $ testProperty "div" $ property $ do
        a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
        b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
        I.div' a b === Just (I.div a b)
      _ -> mzero

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      x === I.with x I.known'

  , case (leRational @l @(0/1), ltRational @(0/1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Rational @('Just '( 'True, l))
                                        @('Just '( 'False, r)))
      _ -> mzero


  , case (leRational @l @(1/1), ltRational @(1/1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @('Just '( 'True, l))
                                        @('Just '( 'False, r)))
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      case I.negate' x of
        Just y -> Just x === I.negate' y
        Nothing -> Nothing === I.from @Rational
                                      @('Just '( 'True, l))
                                      @('Just '( 'False, r))
                                      (negate (I.unwrap x))

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @('Just '( 'False, r))
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))

  ]
  where
    l  = I.min :: I Rational ('Just '( 'True, l)) ('Just '( 'False, r))
    l' = I.unwrap l :: Rational
    r' = KR.rationalVal (Proxy @r) :: Rational

tt'cu
  :: forall (l :: KR.Rational)
  .  I.Interval Rational ('Just '( 'True, l)) 'Nothing
  => TestTree
tt'cu = testGroup ("Interval [" <> show l <> ", infinity)")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @('Just '( 'True, l)) @'Nothing x of
        Nothing -> assert (x < l')
        Just y -> do assert (x >= l')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @('Just '( 'True, l)) @'Nothing x
      I.from (I.unwrap y) === Just y
      if x < l'
         then I.from @Rational @('Just '( 'True, l)) @'Nothing x === Nothing
         else I.from @Rational @('Just '( 'True, l)) @'Nothing x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      b <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x < l')
        Just y -> I.unwrap y === x

  , case leRational @(0/1) @l of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "plus" $ property $ do
        a <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
        b <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
        Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      b <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x < l')
        Just y -> I.unwrap y === x

  , case leRational @(1/1) @l of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "mult" $ property $ do
        a <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
        b <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
        Just (I.mult a b) === I.mult' a b

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      b <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x < l')
        Just y -> I.unwrap y === x

  , if (l' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genRational
      case I.clamp @Rational @('Just '( 'True, l)) @'Nothing x of
        y | x < l' -> I.unwrap y === l'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      x === I.with x I.known'

  , case leRational @l @(0/1) of
      Just Dict -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Rational @('Just '( 'True, l)) @'Nothing)
      _ -> mzero

  , case leRational @l @(1/1) of
      Just Dict -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @('Just '( 'True, l)) @'Nothing)
      _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @('Just '( 'True, l)) @'Nothing
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))
  ]
  where
    l   = I.min        :: I Rational ('Just '( 'True, l)) 'Nothing
    l'  = I.unwrap l   :: Rational

tt'oc
  :: forall (l :: KR.Rational) (r :: KR.Rational)
  .  I.Interval Rational ('Just '( 'False, l)) ('Just '( 'True, r))
  => TestTree
tt'oc = testGroup ("Interval (" <> show l' <> ", " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @('Just '( 'False, l)) @('Just '( 'True, r)) x of
        Nothing -> assert (x <= l' || x > r')
        Just y -> do assert (x > l' && x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @('Just '( 'False, l)) @('Just '( 'True, r)) x
      I.from (I.unwrap y) === Just y
      if x <= l' || x > r'
         then I.from @Rational @('Just '( 'False, l)) @('Just '( 'True, r)) x === Nothing
         else I.from @Rational @('Just '( 'False, l)) @('Just '( 'True, r)) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      b <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x <= l' || x > r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      b <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x <= l' || x > r')
        Just y -> I.unwrap y === x


  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      b <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x <= l' || x > r')
        Just y -> I.unwrap y === x

  , if (r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , case (ltRational @(0/1) @l, leRational @r @(1/1)) of
      (Just Dict, Just Dict) -> pure $ testProperty "div" $ property $ do
        a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
        b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
        I.div' a b === Just (I.div a b)
      _ -> mzero

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      x === I.with x I.known'

  , case (ltRational @l @(0/1), leRational @(0/1) @r) of
      (Just Dict, Just Dict) ->
        pure $ testCase "zero" $
          0 @=? I.unwrap (I.zero @Rational @('Just '( 'False, l))
                                           @('Just '( 'True, r)))
      _ -> mzero

  , case (ltRational @l @(1/1), leRational @(1/1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @('Just '( 'False, l))
                                        @('Just '( 'True, r)))
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      case I.negate' x of
        Just y -> Just x === I.negate' y
        Nothing -> Nothing === I.from @Rational
                                      @('Just '( 'False, l))
                                      @('Just '( 'True, r))
                                      (negate (I.unwrap x))

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'True, r))
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))

  ]
  where
    l' = KR.rationalVal (Proxy @l) :: Rational
    r  = I.max :: I Rational ('Just '( 'False, l)) ('Just '( 'True, r))
    r' = I.unwrap r :: Rational

tt'oo
  :: forall (l :: KR.Rational) (r :: KR.Rational)
  .  I.Interval Rational ('Just '( 'False, l)) ('Just '( 'False, r))
  => TestTree
tt'oo = testGroup ("Interval (" <> show l' <> ", " <> show r' <> ")")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @('Just '( 'False, l)) @('Just '( 'False, r)) x of
        Nothing -> assert (x <= l' || x >= r')
        Just y -> do assert (x > l' && x < r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @('Just '( 'False, l)) @('Just '( 'False, r)) x
      I.from (I.unwrap y) === Just y
      if x <= l' || x >= r'
         then I.from @Rational @('Just '( 'False, l)) @('Just '( 'False, r)) x === Nothing
         else I.from @Rational @('Just '( 'False, l)) @('Just '( 'False, r)) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      b <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x <= l' || x >= r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      b <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x <= l' || x >= r')
        Just y -> I.unwrap y === x

  , case (leRational @(0/1) @l, leRational @r @(1/1)) of
      (Just Dict, Just Dict) ->
        pure $ testProperty "mult" $ property $ do
          a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
          b <- forAll $ genIRational
          Just (I.mult a b) === I.mult' a b
      _ -> mzero

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      b <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x <= l' || x >= r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , case (ltRational @(0/1) @l, leRational @r @(1/1)) of
      (Just Dict, Just Dict) -> pure $ testProperty "div" $ property $ do
        a <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
        b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                                 (genIRational @('Just '( 'False, l))
                                               @('Just '( 'False, r)))
        I.div' a b === Just (I.div a b)
      _ -> mzero

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      x === I.with x I.known'

  , case (ltRational @l @(0/1), ltRational @(0/1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        0 @=? I.unwrap (I.zero @Rational @('Just '( 'False, l))
                                         @('Just '( 'False, r)))
      _ -> mzero

  , case (ltRational @l @(1/1), ltRational @(1/1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @('Just '( 'False, l))
                                        @('Just '( 'False, r)))
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      case I.negate' x of
        Just y -> Just x === I.negate' y
        Nothing -> Nothing === I.from @Rational
                                      @('Just '( 'False, l))
                                      @('Just '( 'False, r))
                                      (negate (I.unwrap x))

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @('Just '( 'False, r))
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))

  ]
  where
    l' = KR.rationalVal (Proxy @l) :: Rational
    r' = KR.rationalVal (Proxy @r) :: Rational

tt'ou
  :: forall (l :: KR.Rational)
  .  I.Interval Rational ('Just '( 'False, l)) 'Nothing
  => TestTree
tt'ou = testGroup ("Interval (" <> show l' <> ", infinity)")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @('Just '( 'False, l)) @'Nothing x of
        Nothing -> assert (x <= l')
        Just y -> do assert (x > l')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @('Just '( 'False, l)) @'Nothing x
      I.from (I.unwrap y) === Just y
      if x <= l'
         then I.from @Rational @('Just '( 'False, l)) @'Nothing x === Nothing
         else I.from @Rational @('Just '( 'False, l)) @'Nothing x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      b <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x <= l')
        Just y -> I.unwrap y === x

  , case leRational @(0/1) @l of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "plus" $ property $ do
        a <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
        b <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
        Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      b <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x <= l')
        Just y -> I.unwrap y === x

  , case leRational @(1/1) @l of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "mult" $ property $ do
        a <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
        b <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
        Just (I.mult a b) === I.mult' a b

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      b <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x <= l')
        Just y -> I.unwrap y === x

  , if (l' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      x === I.with x I.known'

  , case ltRational @l @(0/1) of
      Just Dict -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Rational @('Just '( 'False, l)) @'Nothing)
      _ -> mzero

  , case ltRational @l @(1/1) of
      Just Dict -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @('Just '( 'False, l)) @'Nothing)
      _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @('Just '( 'False, l)) @'Nothing
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))

  ]
  where
    l' = KR.rationalVal (Proxy @l) :: Rational

tt'uc
  :: forall (r :: KR.Rational)
  .  I.Interval Rational 'Nothing ('Just '( 'True, r))
  => TestTree
tt'uc = testGroup ("Interval (-infinity, " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @'Nothing @('Just '( 'True, r)) x of
        Nothing -> assert (x > r')
        Just y -> do assert (x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @'Nothing @('Just '( 'True, r)) x
      I.from (I.unwrap y) === Just y
      if x > r'
         then I.from @Rational @'Nothing @('Just '( 'True, r)) x === Nothing
         else I.from @Rational @'Nothing @('Just '( 'True, r)) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      b <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x > r')
        Just y -> I.unwrap y === x

  , case leRational @r @(0/1) of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "plus" $ property $ do
        a <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
        b <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
        Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      b <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x > r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      b <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x > r')
        Just y -> I.unwrap y === x

  , if (r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genRational
      case I.clamp @Rational @'Nothing @('Just '( 'True, r)) x of
        y | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      x === I.with x I.known'

  , case leRational @(0/1) @r of
      Just Dict -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Rational @'Nothing @('Just '( 'True, r)))
      _ -> mzero

  , case leRational @(1/1) @r of
      Just Dict -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @'Nothing @('Just '( 'True, r)))
      _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @'Nothing @('Just '( 'True, r))
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))
  ]
  where
    r   = I.max        :: I Rational 'Nothing ('Just '( 'True, r))
    r'  = I.unwrap r   :: Rational

tt'uo
  :: forall (r :: KR.Rational)
  .  I.Interval Rational 'Nothing ('Just '( 'False, r))
  => TestTree
tt'uo = testGroup ("Interval (-infinity, " <> show r' <> ")")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @'Nothing @('Just '( 'False, r)) x of
        Nothing -> assert (x >= r')
        Just y -> do assert (x < r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @'Nothing @('Just '( 'False, r)) x
      I.from (I.unwrap y) === Just y
      if x >= r'
         then I.from @Rational @'Nothing @('Just '( 'False, r)) x === Nothing
         else I.from @Rational @'Nothing @('Just '( 'False, r)) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      b <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> assert (x >= r')
        Just y -> I.unwrap y === x

  , case leRational @r @(0/1) of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "plus" $ property $ do
        a <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
        b <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
        Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      b <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> assert (x >= r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      b <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> assert (x >= r')
        Just y -> I.unwrap y === x

  , pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      x === I.with x I.known'

  , case ltRational @(0/1) @r of
      Just Dict -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Rational @'Nothing @('Just '( 'False, r)))
      _ -> mzero

  , case ltRational @(1/1) @r of
      Just Dict -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Rational @'Nothing @('Just '( 'False, r)))
      _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @'Nothing @('Just '( 'False, r))
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))

  ]
  where
    r' = KR.rationalVal (Proxy @r) :: Rational

tt'uu :: TestTree
tt'uu = testGroup "Interval (-infinity, +infinity)"
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genRational
      case I.from @Rational @'Nothing @'Nothing x of
        Nothing -> failure
        Just y -> I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genRational
      let y = I.shove @Rational @'Nothing @'Nothing x
      I.from (I.unwrap y) === Just y
      I.from @Rational @'Nothing @'Nothing x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIRational @'Nothing @'Nothing
      b <- forAll $ genIRational @'Nothing @'Nothing
      let x = I.unwrap a + I.unwrap b
      case I.plus' a b of
        Nothing -> failure
        Just y -> I.unwrap y === x

  , pure $ testProperty "plus" $ property $ do
      a <- forAll $ genIRational @'Nothing @'Nothing
      b <- forAll $ genIRational @'Nothing @'Nothing
      Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIRational @'Nothing @'Nothing
      b <- forAll $ genIRational @'Nothing @'Nothing
      let x = I.unwrap a * I.unwrap b
      case I.mult' a b of
        Nothing -> failure
        Just y -> I.unwrap y === x

  , pure $ testProperty "mult" $ property $ do
      a <- forAll $ genIRational @'Nothing @'Nothing
      b <- forAll $ genIRational @'Nothing @'Nothing
      Just (I.mult a b) === I.mult' a b

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIRational @'Nothing @'Nothing
      b <- forAll $ genIRational @'Nothing @'Nothing
      let x = I.unwrap a - I.unwrap b
      case I.minus' a b of
        Nothing -> failure
        Just y -> I.unwrap y === x

  , pure $ testProperty "minus" $ property $ do
      a <- forAll $ genIRational @'Nothing @'Nothing
      b <- forAll $ genIRational @'Nothing @'Nothing
      Just (I.minus a b) === I.minus' a b

  , pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIRational @'Nothing @'Nothing
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) genIRational
      I.div' a b === I.from (I.unwrap a / I.unwrap b)

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genRational
      x === I.unwrap (I.clamp @Rational @'Nothing @'Nothing x)

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIRational @'Nothing @'Nothing
      x === I.with x I.known'

  , pure $ testCase "zero" $ do
      0 @=? I.unwrap (I.zero @Rational @'Nothing @'Nothing)

  , pure $ testCase "one" $ do
      1 @=? I.unwrap (I.one @Rational @'Nothing @'Nothing)

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIRational @'Nothing @'Nothing
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Rational (I.MinL Rational) (I.MaxR Rational))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIRational @'Nothing @'Nothing
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Rational (I.MinL Rational) (I.MaxR Rational))

  ]

