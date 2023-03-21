{-# LANGUAGE AllowAmbiguousTypes #-}

module I.Test.Word8 (tt) where

import Control.Monad
import Data.Constraint
import Data.Proxy
import Data.Word
import Data.Type.Ord
import GHC.TypeLits qualified as L
import Hedgehog (failure, forAll, property, assert, (===), (/==))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testProperty)

import I (I)
import I qualified

import I.Test.Support

--------------------------------------------------------------------------------

-- checking some constants used below
_tt :: Dict (I.MinL Word8 ~ 0, I.MaxR Word8 ~ 255)
_tt =  Dict

tt :: TestTree
tt = testGroup "Word8"
  [ testProperty "wrap" $ property $ do
      x <- forAll genWord8
      x === I.unwrap (I.wrap x)

  , tt' @0   @0
  , tt' @0   @1
  , tt' @1   @1
  , tt' @100 @100
  , tt' @255 @255
  , tt' @0   @255
  , tt' @0   @100
  , tt' @100 @200
  , tt' @200 @255
  ]

tt'
  :: forall (l :: I.L Word8) (r :: I.R Word8)
  .  I.Inhabited Word8 l r
  => TestTree
tt' = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
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
      a <- forAll $ genIWord8 @l @r
      b <- forAll $ genIWord8 @l @r
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIWord8 @l @r
      b <- forAll $ genIWord8 @l @r
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIWord8 @l @r
      b <- forAll $ genIWord8 @l @r
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , if (l' == 0 && r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIWord8 @l @r
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) (genIWord8 @l @r)
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < l'' || q > r'' || m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genWord8
      case I.clamp @Word8 @l @r x of
        y | x < l' -> I.unwrap y === l'
          | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIWord8 @l @r
      x === I.with x I.known'

  , case L.cmpNat (Proxy @l) (Proxy @r) of
      LTI ->
        [ testProperty "pred'" $ property $ do
            x <- forAll $ genIWord8 @l @r
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1
        , testProperty "succ'" $ property $ do
            x <- forAll $ genIWord8 @l @r
            case I.succ' x of
              Nothing -> x === r
              Just y -> do x /== r
                           I.unwrap y === I.unwrap x + 1
        ]
      _ -> mzero

  , case L.cmpNat (Proxy @l) (Proxy @0) of
      EQI -> pure $ testCase "zero" $
               0 @=? I.unwrap (I.zero @Word8 @l @r)
      _ -> mzero

  , case (leNatural @l @1, leNatural @1 @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Word8 @l @r)
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genIWord8 @l @r
      Nothing === I.negate' x

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIWord8 @l @r
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Word8 (I.MinL Word8) (I.MaxR Word8))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIWord8 @l @r
      x === I.up x
      I.unwrap x === I.unwrap (I.up x :: I Word8 (I.MinL Word8) (I.MaxR Word8))
  ]
  where
    l   = I.min        :: I Word8 l r
    l'  = I.unwrap l   :: Word8
    l'' = toInteger l' :: Integer
    r   = I.max        :: I Word8 l r
    r'  = I.unwrap r   :: Word8
    r'' = toInteger r' :: Integer
