{-# LANGUAGE AllowAmbiguousTypes #-}

module I.Test.Natural (tt) where

import Control.Monad
import Data.Constraint
import Data.Proxy
import Data.Type.Ord
import GHC.TypeLits qualified as L
import Hedgehog (failure, forAll, property, assert, (===), (/==))
import Hedgehog.Gen qualified as Gen
import Numeric.Natural
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testProperty)

import I (I)
import I qualified

import I.Test.Support

--------------------------------------------------------------------------------

tt :: TestTree
tt = testGroup "Natural"
  [ testProperty "wrap" $ property $ do
      x <- forAll genNatural
      x === I.unwrap (I.wrap x)

  , tt'lr @0   @0
  , tt'lr @0   @1
  , tt'lr @0   @100
  , tt'l  @0

  , tt'lr @1   @1
  , tt'lr @1   @100
  , tt'l  @1

  , tt'lr @10  @10
  , tt'lr @10  @100
  , tt'l  @10
  ]

tt'lr
  :: forall (l :: I.L Natural) (r :: Natural)
  .  I.Inhabited Natural l ('Just r)
  => TestTree
tt'lr = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genNatural
      case I.from @Natural @l @('Just r) x of
        Nothing -> assert (x < l' || x > r')
        Just y -> do assert (x >= l' && x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genNatural
      let y = I.shove @Natural @l @('Just r) x
      I.from (I.unwrap y) === Just y
      if x < l' || x > r'
         then I.from @Natural @l @('Just r) x === Nothing
         else I.from @Natural @l @('Just r) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genINatural @l @('Just r)
      b <- forAll $ genINatural @l @('Just r)
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genINatural @l @('Just r)
      b <- forAll $ genINatural @l @('Just r)
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x


  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genINatural @l @('Just r)
      b <- forAll $ genINatural @l @('Just r)
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , if (l' == 0 && r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genINatural @l @('Just r)
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                               (genINatural @l @('Just r))
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < l'' || q > r'' || m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genNatural
      case I.clamp @Natural @l @('Just r) x of
        y | x < l' -> I.unwrap y === l'
          | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genINatural @l @('Just r)
      x === I.with x I.known'


  , case L.cmpNat (Proxy @l) (Proxy @r) of
      LTI ->
        [ testProperty "pred'" $ property $ do
            x <- forAll $ genINatural @l @('Just r)
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1
        , testProperty "succ'" $ property $ do
            x <- forAll $ genINatural @l @('Just r)
            case I.succ' x of
              Nothing -> x === r
              Just y -> do x /== r
                           I.unwrap y === I.unwrap x + 1
        ]
      _ -> mzero

  , case L.cmpNat (Proxy @l) (Proxy @0) of
      EQI -> pure $ testCase "zero" $
        0 @=? I.unwrap (I.zero @Natural @l @('Just r))
      _ -> mzero

  , case (leNatural @l @1, leNatural @1 @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Natural @l @('Just r))
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genINatural @l @('Just r)
      Nothing === I.negate' x

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genINatural @l @('Just r)
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Natural (I.MinL Natural) (I.MaxR Natural))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genINatural @l @('Just r)
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Natural (I.MinL Natural) (I.MaxR Natural))

  ]
  where
    l   = I.min        :: I Natural l ('Just r)
    l'  = I.unwrap l   :: Natural
    l'' = toInteger l' :: Integer
    r   = I.max        :: I Natural l ('Just r)
    r'  = I.unwrap r   :: Natural
    r'' = toInteger r' :: Integer


tt'l
  :: forall (l :: I.L Natural)
  .  I.Inhabited Natural l 'Nothing
  => TestTree
tt'l = testGroup ("Interval [" <> show l <> ", infinity)")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genNatural
      case I.from @Natural @l @'Nothing x of
        Nothing -> assert (x < l')
        Just y -> do assert (x >= l')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genNatural
      let y = I.shove @Natural @l @'Nothing x
      I.from (I.unwrap y) === Just y
      if x < l'
         then I.from @Natural @l @'Nothing x === Nothing
         else I.from @Natural @l @'Nothing x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genINatural @l @'Nothing
      b <- forAll $ genINatural @l @'Nothing
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < l'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genINatural @l @'Nothing
      b <- forAll $ genINatural @l @'Nothing
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'')
        Just y -> toInteger (I.unwrap y) === x


  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genINatural @l @'Nothing
      b <- forAll $ genINatural @l @'Nothing
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < l'')
        Just y -> toInteger (I.unwrap y) === x

  , if (l' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genINatural @l @'Nothing
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                               (genINatural @l @'Nothing)
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < l'' || m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genNatural
      case I.clamp @Natural @l @'Nothing x of
        y | x < l' -> I.unwrap y === l'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      x === I.with x I.known'

  , pure $ testProperty "pred'" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      case I.pred' x of
        Nothing -> x === l
        Just y -> do x /== l
                     I.unwrap y === I.unwrap x - 1

  , pure $ testProperty "succ'" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      case I.succ' x of
        Nothing -> failure
        Just y -> I.unwrap y === I.unwrap x + 1

  , pure $ testProperty "succ" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      Just (I.succ x) === I.succ' x

  , case L.cmpNat (Proxy @l) (Proxy @0) of
      EQI -> pure $ testCase "zero" $
               0 @=? I.unwrap (I.zero @Natural @l @'Nothing)
      _ -> mzero

  , case leNatural @l @1 of
      Just Dict -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Natural @l @'Nothing)
      _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Natural (I.MinL Natural) (I.MaxR Natural))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Natural (I.MinL Natural) (I.MaxR Natural))

  ]
  where
    l   = I.min        :: I Natural l 'Nothing
    l'  = I.unwrap l   :: Natural
    l'' = toInteger l' :: Integer

