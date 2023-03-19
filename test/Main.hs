{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module Main (main) where

import Control.Monad
import Data.Constraint
import Data.Int
import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Word
import Data.Type.Ord
import GHC.Real (Ratio((:%)))
import GHC.TypeNats
import Hedgehog (MonadGen, forAll, property, assert, diff, (===), (/==))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=), (@=?))
import Test.Tasty.Hedgehog (HedgehogTestLimit (..), testProperty)
import qualified Test.Tasty.Runners as Tasty

import I (I)
import I qualified

--------------------------------------------------------------------------------

main :: IO ()
main =
  Tasty.defaultMainWithIngredients
    [ Tasty.consoleTestReporter, Tasty.listingTests ]
    $ Tasty.localOption (HedgehogTestLimit (Just 5000))
    $ tt

tt :: TestTree
tt = testGroup "I"
  [ tt_Word8
  ]

-- checking some constants used below
_tt_Word8 :: Dict (I.MinL Word8 ~ 0, I.MaxR Word8 ~ 255)
_tt_Word8 =  Dict

tt_Word8 :: TestTree
tt_Word8 = testGroup "Word8"
  [ testProperty "wrap" $ property $ do
      x <- forAll genWord8
      x === I.unwrap (I.wrap x)

  , testProperty "from (full range)" $ property $ do
      x <- forAll genWord8
      Just x === fmap I.unwrap (I.from @Word8 @0 @255 x)

  , testProperty "from (partial range on left)" $ property $ do
      x <- forAll genWord8
      case I.from @_ @0 @9 x of
        Nothing -> diff x (>) 9
        Just y -> do diff x (<=) 9
                     I.unwrap y === x

  , testProperty "from (partial range on right)" $ property $ do
      x <- forAll genWord8
      case I.from @Word8 @250 @255 x of
        Nothing -> diff x (<) 250
        Just y -> do diff x (>=) 250
                     I.unwrap y === x

  , testProperty "from (partial range on center)" $ property $ do
      x <- forAll genWord8
      case I.from @Word8 @50 @70 x of
        Nothing -> assert (x < 50 || x > 70)
        Just y -> do assert (x >= 50 && x <= 70)
                     I.unwrap y === x

  , testProperty "plus' (partial range on left)" $ property $ do
      a <- forAll $ genI @Word8 @0 @100
      b <- forAll $ genI @Word8 @0 @100
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> diff x (>) 100
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "mult' (partial range on left)" $ property $ do
      a <- forAll $ genI @Word8 @0 @100
      b <- forAll $ genI @Word8 @0 @100
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> diff x (>) 100
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "minus' (partial range on left)" $ property $ do
      a <- forAll $ genI @Word8 @0 @100
      b <- forAll $ genI @Word8 @0 @100
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < 0 || x > 100)
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "div' (partial range on left)" $ property $ do
      a <- forAll $ genI @Word8 @0 @100
      b <- forAll $ I.up <$> genI @Word8 @1 @100
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < 0 || m > 100 || m /= 0 || I.unwrap b == 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0
                     I.unwrap b /== 0

  , testProperty "plus' (partial range on right)" $ property $ do
      a <- forAll $ genI @Word8 @100 @255
      b <- forAll $ genI @Word8 @100 @255
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < 100 || x > 255)
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "mult' (partial range on right)" $ property $ do
      a <- forAll $ genI @Word8 @100 @255
      b <- forAll $ genI @Word8 @100 @255
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < 100 || x > 255)
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "minus' (partial range on right)" $ property $ do
      a <- forAll $ genI @Word8 @100 @255
      b <- forAll $ genI @Word8 @100 @255
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < 100 || x > 255)
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "div' (partial range on left)" $ property $ do
      a <- forAll $ genI @Word8 @100 @255
      b <- forAll $ genI @Word8 @100 @255
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < 100 || m > 255 || m /= 0 || I.unwrap b == 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0
                     I.unwrap b /== 0

  , testProperty "plus' (partial range on center)" $ property $ do
      a <- forAll $ genI @Word8 @50 @200
      b <- forAll $ genI @Word8 @50 @200
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < 50 || x > 200)
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "mult' (partial range on center)" $ property $ do
      a <- forAll $ genI @Word8 @50 @200
      b <- forAll $ genI @Word8 @50 @200
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < 50 || x > 200)
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "minus' (partial range on center)" $ property $ do
      a <- forAll $ genI @Word8 @50 @200
      b <- forAll $ genI @Word8 @50 @200
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < 50 || x > 200)
        Just y -> toInteger (I.unwrap y) === x

  , testProperty "div' (partial range on left)" $ property $ do
      a <- forAll $ genI @Word8 @50 @200
      b <- forAll $ genI @Word8 @50 @200
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < 50 || m > 200 || m /= 0 || I.unwrap b == 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0
                     I.unwrap b /== 0
  ]

--------------------------------------------------------------------------------

genWord8 :: MonadGen m => m Word8
genWord8 = Gen.integral $ Range.constant minBound maxBound

--------------------------------------------------------------------------------

class GenI x where
  genI :: forall l r m. (MonadGen m, I.Shove x l r) => m (I x l r)

instance GenI Int8 where
  genI = fmap I.shove $ Gen.integral $ Range.constant minBound maxBound

instance GenI Word8 where
  genI = fmap I.shove $ Gen.integral $ Range.constant minBound maxBound

instance GenI Natural where
  genI = fmap I.shove $ Gen.integral $ Range.linear 0 (10 ^ (100 :: Int))

instance GenI Integer where
  genI = fmap I.shove $ Gen.integral $
    Range.linearFrom 0 (negate (10 ^ (100 :: Int))) (10 ^ (100 :: Int))

instance GenI Rational where
  genI = do
    n <- Gen.integral $ Range.linearFrom 0 (negate (10 ^ (100 :: Int)))
                                           (10 ^ (100 :: Int))
    d <- Gen.integral $ Range.linear 1 (10 ^ (100 :: Int))
    pure $ I.shove (n :% d)

