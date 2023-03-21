{-# LANGUAGE AllowAmbiguousTypes #-}

module I.Test.Integer (tt) where

import Control.Monad
import Data.Constraint
import Data.Proxy
import Data.Type.Ord
import Hedgehog (failure, forAll, property, assert, (===), (/==))
import qualified Hedgehog.Gen as Gen
import KindInteger (N, P)
import KindInteger qualified as KI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testProperty)

import I (I)
import I qualified

import I.Test.Support

--------------------------------------------------------------------------------

tt :: TestTree
tt = testGroup "Integer"
  [ testProperty "wrap" $ property $ do
      x <- forAll genInteger
      x === I.unwrap (I.wrap x)

  , tt'lr @(N 100) @(N 100)
  , tt'lr @(N 100) @(N 10)
  , tt'lr @(N 100) @(N 1)
  , tt'lr @(N 100) @(N 0)
  , tt'lr @(N 1)   @(N 1)
  , tt'lr @(N 1)   @(P 0)
  , tt'lr @(P 0)   @(P 0)
  , tt'lr @(P 0)   @(P 1)
  , tt'lr @(P 1)   @(P 1)
  , tt'lr @(P 0)   @(P 100)
  , tt'lr @(P 1)   @(P 100)
  , tt'lr @(P 10)  @(P 100)
  , tt'lr @(P 100) @(P 100)

  , tt'lu  @(N 100)
  , tt'lu  @(N 1)
  , tt'lu  @(P 0)
  , tt'lu  @(P 1)
  , tt'lu  @(P 100)

  , tt'ur  @(N 100)
  , tt'ur  @(N 1)
  , tt'ur  @(P 0)
  , tt'ur  @(P 1)
  , tt'ur  @(P 100)

  , tt'uu
  ]

tt'lr
  :: forall (l :: KI.Integer) (r :: KI.Integer)
  .  I.Inhabited Integer ('Just l) ('Just r)
  => TestTree
tt'lr = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genInteger
      case I.from @Integer @('Just l) @('Just r) x of
        Nothing -> assert (x < l' || x > r')
        Just y -> do assert (x >= l' && x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genInteger
      let y = I.shove @Integer @('Just l) @('Just r) x
      I.from (I.unwrap y) === Just y
      if x < l' || x > r'
         then I.from @Integer @('Just l) @('Just r) x === Nothing
         else I.from @Integer @('Just l) @('Just r) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @('Just r)
      b <- forAll $ genIInteger @('Just l) @('Just r)
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @('Just r)
      b <- forAll $ genIInteger @('Just l) @('Just r)
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x


  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @('Just r)
      b <- forAll $ genIInteger @('Just l) @('Just r)
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < l'' || x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , if (l' == 0 && r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @('Just r)
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                               (genIInteger @('Just l) @('Just r))
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < l'' || q > r'' || m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genInteger
      case I.clamp @Integer @('Just l) @('Just r) x of
        y | x < l' -> I.unwrap y === l'
          | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIInteger @('Just l) @('Just r)
      x === I.with x I.known'


  , case KI.cmpInteger (Proxy @l) (Proxy @r) of
      LTI ->
        [ testProperty "pred'" $ property $ do
            x <- forAll $ genIInteger @('Just l) @('Just r)
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1

        , testProperty "succ'" $ property $ do
            x <- forAll $ genIInteger @('Just l) @('Just r)
            case I.succ' x of
              Nothing -> x === r
              Just y -> do x /== r
                           I.unwrap y === I.unwrap x + 1
        ]
      _ -> mzero

  , case KI.cmpInteger (Proxy @l) (Proxy @(P 0)) of
      EQI -> pure $ testCase "zero" $
               0 @=? I.unwrap (I.zero @Integer @('Just l) @('Just r))
      _ -> mzero


  , case (leInteger @l @(P 1), leInteger @(P 1) @r) of
      (Just Dict, Just Dict) -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Integer @('Just l) @('Just r))
      _ -> mzero

  , pure $ testProperty "negate'" $ property $ do
      x <- forAll $ genIInteger @('Just l) @('Just r)
      case I.negate' x of
        Just y -> Just x === I.negate' y
        Nothing -> Nothing === I.from @Integer @('Just l) @('Just r)
                                      (negate (toInteger (I.unwrap x)))

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIInteger @('Just l) @('Just r)
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Integer (I.MinL Integer) (I.MaxR Integer))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIInteger @('Just l) @('Just r)
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Integer (I.MinL Integer) (I.MaxR Integer))

  ]
  where
    l   = I.min        :: I Integer ('Just l) ('Just r)
    l'  = I.unwrap l   :: Integer
    l'' = toInteger l' :: Integer
    r   = I.max        :: I Integer ('Just l) ('Just r)
    r'  = I.unwrap r   :: Integer
    r'' = toInteger r' :: Integer


tt'lu
  :: forall (l :: KI.Integer)
  .  I.Inhabited Integer ('Just l) 'Nothing
  => TestTree
tt'lu = testGroup ("Interval [" <> show l <> ", infinity)")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genInteger
      case I.from @Integer @('Just l) @'Nothing x of
        Nothing -> assert (x < l')
        Just y -> do assert (x >= l')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genInteger
      let y = I.shove @Integer @('Just l) @'Nothing x
      I.from (I.unwrap y) === Just y
      if x < l'
         then I.from @Integer @('Just l) @'Nothing x === Nothing
         else I.from @Integer @('Just l) @'Nothing x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @'Nothing
      b <- forAll $ genIInteger @('Just l) @'Nothing
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x < l'')
        Just y -> toInteger (I.unwrap y) === x

  , case leInteger @(P 0) @l of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "plus" $ property $ do
        a <- forAll $ genIInteger @('Just l) @'Nothing
        b <- forAll $ genIInteger @('Just l) @'Nothing
        Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @'Nothing
      b <- forAll $ genIInteger @('Just l) @'Nothing
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'')
        Just y -> toInteger (I.unwrap y) === x

  , case leInteger @(P 0) @l of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "mult" $ property $ do
        a <- forAll $ genIInteger @('Just l) @'Nothing
        b <- forAll $ genIInteger @('Just l) @'Nothing
        Just (I.mult a b) === I.mult' a b

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @'Nothing
      b <- forAll $ genIInteger @('Just l) @'Nothing
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x < l'')
        Just y -> toInteger (I.unwrap y) === x

  , if (l' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @'Nothing
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                               (genIInteger @('Just l) @'Nothing)
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q < l'' || m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genInteger
      case I.clamp @Integer @('Just l) @'Nothing x of
        y | x < l' -> I.unwrap y === l'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      x === I.with x I.known'

  , pure $ testProperty "pred'" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      case I.pred' x of
        Nothing -> x === l
        Just y -> do x /== l
                     I.unwrap y === I.unwrap x - 1

  , pure $ testProperty "succ'" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      case I.succ' x of
        Nothing -> failure
        Just y -> do I.unwrap y === I.unwrap x + 1

  , pure $ testProperty "succ" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      Just (I.succ x) === I.succ' x

  , case leInteger @l @(P 0) of
      Just Dict -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Integer @('Just l) @'Nothing)
      _ -> mzero

  , case leInteger @l @(P 1) of
      Just Dict -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Integer @('Just l) @'Nothing)
      _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Integer (I.MinL Integer) (I.MaxR Integer))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Integer (I.MinL Integer) (I.MaxR Integer))

  ]
  where
    l   = I.min        :: I Integer ('Just l) 'Nothing
    l'  = I.unwrap l   :: Integer
    l'' = toInteger l' :: Integer

tt'ur
  :: forall (r :: KI.Integer)
  .  I.Inhabited Integer 'Nothing ('Just r)
  => TestTree
tt'ur = testGroup ("Interval (-infinity, " <> show r <> "]")
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genInteger
      case I.from @Integer @'Nothing @('Just r) x of
        Nothing -> assert (x > r')
        Just y -> do assert (x <= r')
                     I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genInteger
      let y = I.shove @Integer @'Nothing @('Just r) x
      I.from (I.unwrap y) === Just y
      if x > r'
         then I.from @Integer @'Nothing @('Just r) x === Nothing
         else I.from @Integer @'Nothing @('Just r) x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @('Just r)
      b <- forAll $ genIInteger @'Nothing @('Just r)
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> assert (x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , case leInteger @r @(P 0) of
      Nothing -> mzero
      Just Dict -> pure $ testProperty "plus" $ property $ do
        a <- forAll $ genIInteger @'Nothing @('Just r)
        b <- forAll $ genIInteger @'Nothing @('Just r)
        Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @('Just r)
      b <- forAll $ genIInteger @'Nothing @('Just r)
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @('Just r)
      b <- forAll $ genIInteger @'Nothing @('Just r)
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> assert (x > r'')
        Just y -> toInteger (I.unwrap y) === x

  , if (r' == 0) then mzero else
    pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @('Just r)
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                               (genIInteger @'Nothing @('Just r))
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (q > r'' || m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genInteger
      case I.clamp @Integer @'Nothing @('Just r) x of
        y | x > r' -> I.unwrap y === r'
          | otherwise -> Just y === I.from x

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIInteger @'Nothing @('Just r)
      x === I.with x I.known'

  , pure $ testProperty "pred'" $ property $ do
      x <- forAll $ genIInteger @'Nothing @('Just r)
      case I.pred' x of
        Nothing -> failure
        Just y -> I.unwrap y === I.unwrap x - 1

  , pure $ testProperty "succ'" $ property $ do
      x <- forAll $ genIInteger @'Nothing @('Just r)
      case I.succ' x of
        Nothing -> x === r
        Just y -> do I.unwrap y === I.unwrap x + 1

  , pure $ testProperty "pred" $ property $ do
      x <- forAll $ genIInteger @'Nothing @('Just r)
      Just (I.pred x) === I.pred' x

  , case leInteger @(P 0) @r of
      Just Dict -> pure $ testCase "zero" $ do
        0 @=? I.unwrap (I.zero @Integer @'Nothing @('Just r))
      _ -> mzero

  , case leInteger @(P 1) @r of
      Just Dict -> pure $ testCase "one" $ do
        1 @=? I.unwrap (I.one @Integer @'Nothing @('Just r))
      _ -> mzero

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIInteger @'Nothing @('Just r)
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Integer (I.MinL Integer) (I.MaxR Integer))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIInteger @'Nothing @('Just r)
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Integer (I.MinL Integer) (I.MaxR Integer))

  ]
  where
    r   = I.max        :: I Integer 'Nothing ('Just r)
    r'  = I.unwrap r   :: Integer
    r'' = toInteger r' :: Integer

tt'uu :: TestTree
tt'uu = testGroup "Interval (-infinity, +infinity)"
  $ concat
  [ pure $ testProperty "from" $ property $ do
      x <- forAll genInteger
      case I.from @Integer @'Nothing @'Nothing x of
        Nothing -> failure
        Just y -> I.unwrap y === x

  , pure $ testProperty "shove" $ property $ do
      x <- forAll genInteger
      let y = I.shove @Integer @'Nothing @'Nothing x
      I.from (I.unwrap y) === Just y
      I.from @Integer @'Nothing @'Nothing x /== Nothing

  , pure $ testProperty "plus'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @'Nothing
      b <- forAll $ genIInteger @'Nothing @'Nothing
      let x = toInteger (I.unwrap a) + toInteger (I.unwrap b)
      case I.plus' a b of
        Nothing -> failure
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "plus" $ property $ do
      a <- forAll $ genIInteger @'Nothing @'Nothing
      b <- forAll $ genIInteger @'Nothing @'Nothing
      Just (I.plus a b) === I.plus' a b

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @'Nothing
      b <- forAll $ genIInteger @'Nothing @'Nothing
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> failure
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "mult" $ property $ do
      a <- forAll $ genIInteger @'Nothing @'Nothing
      b <- forAll $ genIInteger @'Nothing @'Nothing
      Just (I.mult a b) === I.mult' a b

  , pure $ testProperty "minus'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @'Nothing
      b <- forAll $ genIInteger @'Nothing @'Nothing
      let x = toInteger (I.unwrap a) - toInteger (I.unwrap b)
      case I.minus' a b of
        Nothing -> failure
        Just y -> toInteger (I.unwrap y) === x

  , pure $ testProperty "minus" $ property $ do
      a <- forAll $ genIInteger @'Nothing @'Nothing
      b <- forAll $ genIInteger @'Nothing @'Nothing
      Just (I.minus a b) === I.minus' a b

  , pure $ testProperty "div'" $ property $ do
      a <- forAll $ genIInteger @'Nothing @'Nothing
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0)
                               (genIInteger @'Nothing @'Nothing)
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      case I.div' a b of
        Nothing -> assert (m /= 0)
        Just y -> do q === toInteger (I.unwrap y)
                     m === 0

  , pure $ testProperty "clamp'" $ property $ do
      x <- forAll $ genInteger
      x === I.unwrap (I.clamp @Integer @'Nothing @'Nothing x)

  , pure $ testProperty "with" $ property $ do
      x <- forAll $ genIInteger @'Nothing @'Nothing
      x === I.with x I.known'

  , pure $ testProperty "pred'" $ property $ do
      x <- forAll $ genIInteger @'Nothing @'Nothing
      case I.pred' x of
        Nothing -> failure
        Just y -> I.unwrap y === I.unwrap x - 1

  , pure $ testProperty "succ'" $ property $ do
      x <- forAll $ genIInteger @'Nothing @'Nothing
      case I.succ' x of
        Nothing -> failure
        Just y -> I.unwrap y === I.unwrap x + 1

  , pure $ testProperty "pred" $ property $ do
      x <- forAll $ genIInteger @'Nothing @'Nothing
      Just (I.pred x) === I.pred' x

  , pure $ testProperty "succ" $ property $ do
      x <- forAll $ genIInteger @'Nothing @'Nothing
      Just (I.succ x) === I.succ' x

  , pure $ testCase "zero" $ do
      0 @=? I.unwrap (I.zero @Integer @'Nothing @'Nothing)

  , pure $ testCase "one" $ do
      1 @=? I.unwrap (I.one @Integer @'Nothing @'Nothing)

  , pure $ testProperty "down" $ property $ do
      x <- forAll $ genIInteger @'Nothing @'Nothing
      Just x === I.down x
      case I.down x of
        Nothing -> failure
        Just y -> I.unwrap x
              === I.unwrap (y :: I Integer (I.MinL Integer) (I.MaxR Integer))

  , pure $ testProperty "up" $ property $ do
      x <- forAll $ genIInteger @'Nothing @'Nothing
      x === I.up x
      I.unwrap x ===
        I.unwrap (I.up x :: I Integer (I.MinL Integer) (I.MaxR Integer))

  ]

