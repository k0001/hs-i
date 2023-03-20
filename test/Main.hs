{-# LANGUAGE AllowAmbiguousTypes #-}
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

--------------------------------------------------------------------------------

-- checking some constants used below
_tt_Word8 :: Dict (I.MinL Word8 ~ 0, I.MaxR Word8 ~ 255)
_tt_Word8 =  Dict

tt_Word8 :: TestTree
tt_Word8 = testGroup "Word8"
  [ testProperty "wrap" $ property $ do
      x <- forAll genWord8
      x === I.unwrap (I.wrap x)

  , testCase "zero" $ do
      0 @=? I.unwrap (I.zero @Word8 @0 @0)
      0 @=? I.unwrap (I.zero @Word8 @0 @100)
      0 @=? I.unwrap (I.zero @Word8 @0 @255)

  , testCase "one" $ do
      1 @=? I.unwrap (I.one @Word8 @0 @1)
      1 @=? I.unwrap (I.one @Word8 @0 @100)
      1 @=? I.unwrap (I.one @Word8 @0 @255)

      1 @=? I.unwrap (I.one @Word8 @1 @1)
      1 @=? I.unwrap (I.one @Word8 @1 @100)
      1 @=? I.unwrap (I.one @Word8 @1 @255)

  , tt_Word8' @0   @0
  , tt_Word8' @100 @100
  , tt_Word8' @255 @255
  , tt_Word8' @0   @255
  , tt_Word8' @0   @100
  , tt_Word8' @100 @200
  , tt_Word8' @200 @255
  ]

tt_Word8'
  :: forall (l :: I.L Word8) (r :: I.R Word8)
  .  I.Inhabited Word8 l r
  => TestTree
tt_Word8' = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genWord8
      case I.from @Word8 @l @r x of
        Nothing -> assert (x < l' || x > r')
        Just y -> do assert (x >= l' && x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genWord8
      let y = I.shove @Word8 @l @r x
      I.from (I.unwrap y) === Just y
      if x < l' || x > r'
         then I.from @Word8 @l @r x === Nothing
         else I.from @Word8 @l @r x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genI @Word8 @l @r
      b <- forAll $ genI @Word8 @l @r
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genI @Word8 @l @r
      b <- forAll $ genI @Word8 @l @r
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genI @Word8 @l @r
      b <- forAll $ genI @Word8 @l @r
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , case cmpNat (Proxy @0) (Proxy @r) of
      LTI -> pure $ testProperty "div'" $ property $ do
        a <- forAll $ genI @Word8 @l @r
        b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) (genI @Word8 @l @r)
        let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
        case I.div' a b of
          Nothing -> assert (q < l'' || m > r'' || m /= 0 || I.unwrap b == 0)
          Just y -> do q === toInteger (I.unwrap y)
                       m === 0
                       I.unwrap b /== 0
      _ -> mzero

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genWord8
      case I.clamp @Word8 @l @r x of
        y | x < l' -> I.unwrap y === l'
          | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genI @Word8 @l @r
      x === I.with x I.known'

  , case cmpNat (Proxy @l) (Proxy @r) of
      LTI ->
        [ testProperty "pred" $ property $ do
            x <- forAll $ genI @Word8 @l @r
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1
        , testProperty "succ" $ property $ do
            x <- forAll $ genI @Word8 @l @r
            case I.succ' x of
              Nothing -> x === r
              Just y -> do x /== r
                           I.unwrap y === I.unwrap x + 1
        ]
      _ -> mzero

  ]
  where
    l   = I.min        :: I Word8 l r
    l'  = I.unwrap l   :: Word8
    l'' = toInteger l' :: Integer
    r   = I.max        :: I Word8 l r
    r'  = I.unwrap r   :: Word8
    r'' = toInteger r' :: Integer

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

