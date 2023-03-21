{-# LANGUAGE AllowAmbiguousTypes #-}

module I.Test.Int8 (tt) where

import Control.Monad
import Data.Bits
import Data.Constraint
import Data.Int
import Data.Proxy
import Data.Type.Ord
import Hedgehog (failure, forAll, property, assert, (===), (/==))
import Hedgehog.Gen qualified as Gen
import KindInteger (N, P)
import KindInteger qualified as KI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testProperty)

import I (I)
import I qualified

import I.Test.Support

--------------------------------------------------------------------------------

-- checking some constants used below
_tt :: Dict (I.MinL Int8 ~ N 128, I.MaxR Int8 ~ P 127)
_tt =  Dict

tt :: TestTree
tt = testGroup "Int8"
  [ testProperty "wrap" $ property $ do
      x <- forAll genInt8
      x === I.unwrap (I.wrap x)

  , tt' @(N 128) @(P 127) -- full range

  , tt' @(N 1)   @(N 1)
  , tt' @(N 1)   @(P 0)
  , tt' @(P 0)   @(P 0)
  , tt' @(P 0)   @(P 1)
  , tt' @(P 1)   @(P 1)

  , tt' @(N 128) @(N 128) -- left end
  , tt' @(P 127) @(P 127) -- right end

  , tt' @(N 128) @(N 100) -- partial on the left, some negatives
  , tt' @(N 128) @(N   1) -- partial on the left, all negatives
  , tt' @(N 128) @(N   0) -- partial on the left, all negatives and zero
  , tt' @(N 128) @(P  50) -- partial on the left, negative and positive

  , tt' @(P 100) @(P 127) -- partial on the right, some positives
  , tt' @(P   1) @(P 127) -- partial on the right, all positives
  , tt' @(P   0) @(P 127) -- partial on the right, all positives and zero
  , tt' @(N  50) @(P 127) -- partial on the right, negative and positive

  , tt' @(N 100) @(N   1) -- partial on the center, negatives
  , tt' @(N 100) @(N   0) -- partial on the center, negatives and zero
  , tt' @(P   1) @(P 100) -- partial on the center, positives
  , tt' @(N   0) @(P 100) -- partial on the center, positives and zero
  , tt' @(N 100) @(P 100) -- partial on the center, negative and positive

  ]

tt'
  :: forall (l :: I.L Int8) (r :: I.R Int8)
  .  I.Inhabited Int8 l r
  => TestTree
tt' = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genInt8
      case I.from @Int8 @l @r x of
        Nothing -> assert (x < l' || x > r')
        Just y -> do assert (x >= l' && x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genInt8
      let y = I.shove @Int8 @l @r x
      I.from (I.unwrap y) === Just y
      if x < l' || x > r'
         then I.from @Int8 @l @r x === Nothing
         else I.from @Int8 @l @r x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIInt8 @l @r
      b <- forAll $ genIInt8 @l @r
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIInt8 @l @r
      b <- forAll $ genIInt8 @l @r
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIInt8 @l @r
      b <- forAll $ genIInt8 @l @r
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , if (l' == 0 && r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIInt8 @l @r
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) (genIInt8 @l @r)
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < l'' || q > r'' || m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genInt8
      case I.clamp @Int8 @l @r x of
        y | x < l' -> I.unwrap y === l'
          | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIInt8 @l @r
      x === I.with x I.known'

  , case KI.cmpInteger (Proxy @l) (Proxy @r) of
      LTI ->
        [ testProperty "pred'" $ property $ do
            x <- forAll $ genIInt8 @l @r
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1
        , testProperty "succ'" $ property $ do
            x <- forAll $ genIInt8 @l @r
            case I.succ' x of
              Nothing -> x === r
              Just y -> do x /== r
                           I.unwrap y === I.unwrap x + 1
        ]
      _ -> mzero

  , case (leInteger @l @(P 0), leInteger @(P 0) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Int8 @l @r)
      _ -> mzero

  , case (leInteger @l @(P 1), leInteger @(P 1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Int8 @l @r)
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genIInt8 @l @r
      I.negate' x ===
        (I.from =<< toIntegralSized (negate (toInteger (I.unwrap x))))

  , withDict (negateInteger @r) $
      case (leInteger @l @(P 0), leInteger @(P 0) @r) of
        (Just Dict, Just Dict) ->
          case KI.cmpInteger (Proxy @l) (Proxy @(KI.Negate r)) of
            EQI -> pure $ testProperty "negate" $ property $ do
              x <- forAll $ genIInt8 @l @r
              Just (I.negate x) === I.negate' x
            _ -> mzero
        _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIInt8 @l @r
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Int8 (I.MinL Int8) (I.MaxR Int8))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIInt8 @l @r
      x === I.up x
      I.unwrap x === I.unwrap (I.up x :: I Int8 (I.MinL Int8) (I.MaxR Int8))
  ]
  where
    l   = I.min        :: I Int8 l r
    l'  = I.unwrap l   :: Int8
    l'' = toInteger l' :: Integer
    r   = I.max        :: I Int8 l r
    r'  = I.unwrap r   :: Int8
    r'' = toInteger r' :: Integer


