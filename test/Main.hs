{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module Main (main) where

import Control.Exception qualified as Ex
import Control.Monad
import Data.Bits
import Data.Constraint
import Data.Int
import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Word
import Data.Type.Ord
import GHC.Real (Ratio((:%)))
import GHC.TypeLits qualified as L
import Hedgehog (MonadGen, failure, annotateShow, forAll, property, assert,
  diff, (===), (/==))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import KindInteger (N, P)
import KindInteger qualified as KI
import KindRational qualified as KR
import Numeric.Natural
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=), (@=?))
import Test.Tasty.Hedgehog (HedgehogTestLimit (..), testProperty)
import qualified Test.Tasty.Runners as Tasty
import Unsafe.Coerce (unsafeCoerce)

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
  , tt_Int8
  , tt_Natural
  , tt_Integer
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

  , tt_Word8' @0   @0
  , tt_Word8' @0   @1
  , tt_Word8' @1   @1
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
        [ testProperty "pred" $ property $ do
            x <- forAll $ genIWord8 @l @r
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1
        , testProperty "succ" $ property $ do
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

--------------------------------------------------------------------------------

-- checking some constants used below
_tt_Int8 :: Dict (I.MinL Int8 ~ N 128, I.MaxR Int8 ~ P 127)
_tt_Int8 =  Dict

tt_Int8 :: TestTree
tt_Int8 = testGroup "Int8"
  [ testProperty "wrap" $ property $ do
      x <- forAll genInt8
      x === I.unwrap (I.wrap x)

  , tt_Int8' @(N 128) @(P 127) -- full range

  , tt_Int8' @(N 1)   @(N 1)
  , tt_Int8' @(N 1)   @(P 0)
  , tt_Int8' @(P 0)   @(P 0)
  , tt_Int8' @(P 0)   @(P 1)
  , tt_Int8' @(P 1)   @(P 1)

  , tt_Int8' @(N 128) @(N 128) -- left end
  , tt_Int8' @(P 127) @(P 127) -- right end

  , tt_Int8' @(N 128) @(N 100) -- partial on the left, some negatives
  , tt_Int8' @(N 128) @(N   1) -- partial on the left, all negatives
  , tt_Int8' @(N 128) @(N   0) -- partial on the left, all negatives and zero
  , tt_Int8' @(N 128) @(P  50) -- partial on the left, negative and positive

  , tt_Int8' @(P 100) @(P 127) -- partial on the right, some positives
  , tt_Int8' @(P   1) @(P 127) -- partial on the right, all positives
  , tt_Int8' @(P   0) @(P 127) -- partial on the right, all positives and zero
  , tt_Int8' @(N  50) @(P 127) -- partial on the right, negative and positive

  , tt_Int8' @(N 100) @(N   1) -- partial on the center, negatives
  , tt_Int8' @(N 100) @(N   0) -- partial on the center, negatives and zero
  , tt_Int8' @(P   1) @(P 100) -- partial on the center, positives
  , tt_Int8' @(N   0) @(P 100) -- partial on the center, positives and zero
  , tt_Int8' @(N 100) @(P 100) -- partial on the center, negative and positive

  ]

tt_Int8'
  :: forall (l :: I.L Int8) (r :: I.R Int8)
  .  I.Inhabited Int8 l r
  => TestTree
tt_Int8' = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
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
      annotateShow ('l', l)
      annotateShow ('r', r)
      a <- forAll $ genIInt8 @l @r
      annotateShow ('a', a)
      b <- forAll $ Gen.filter (\x -> I.unwrap x /= 0) (genIInt8 @l @r)
      annotateShow ('b', b)
      let (q, m) = toInteger (I.unwrap a) `divMod` toInteger (I.unwrap b)
      annotateShow ('q', q)
      annotateShow ('m', m)
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
        [ testProperty "pred" $ property $ do
            x <- forAll $ genIInt8 @l @r
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1
        , testProperty "succ" $ property $ do
            x <- forAll $ genIInt8 @l @r
            case I.succ' x of
              Nothing -> x === r
              Just y -> do x /== r
                           I.unwrap y === I.unwrap x + 1
        ]
      _ -> mzero

  , case KI.cmpInteger (Proxy @l) (Proxy @(P 0)) of
      EQI -> pure $ testCase "zero" $
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

--------------------------------------------------------------------------------

tt_Natural :: TestTree
tt_Natural = testGroup "Natural"
  [ testProperty "wrap" $ property $ do
      x <- forAll genNatural
      x === I.unwrap (I.wrap x)

  , tt_Natural'lr @0   @0
  , tt_Natural'lr @0   @1
  , tt_Natural'lr @0   @100
  , tt_Natural'l  @0

  , tt_Natural'lr @1   @1
  , tt_Natural'lr @1   @100
  , tt_Natural'l  @1

  , tt_Natural'lr @10  @10
  , tt_Natural'lr @10  @100
  , tt_Natural'l  @10
  ]

tt_Natural'lr
  :: forall (l :: I.L Natural) (r :: Natural)
  .  I.Inhabited Natural l ('Just r)
  => TestTree
tt_Natural'lr = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
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
        [ testProperty "pred" $ property $ do
            x <- forAll $ genINatural @l @('Just r)
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1
        , testProperty "succ" $ property $ do
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


tt_Natural'l
  :: forall (l :: I.L Natural)
  .  I.Inhabited Natural l 'Nothing
  => TestTree
tt_Natural'l = testGroup ("Interval [" <> show l <> ", infinity)")
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

  , pure $ testProperty "pred" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      case I.pred' x of
        Nothing -> x === l
        Just y -> do x /== l
                     I.unwrap y === I.unwrap x - 1

  , pure $ testProperty "succ" $ property $ do
      x <- forAll $ genINatural @l @'Nothing
      case I.succ' x of
        Nothing -> failure
        Just y -> do I.unwrap y === I.unwrap x + 1

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

--------------------------------------------------------------------------------

tt_Integer :: TestTree
tt_Integer = testGroup "Integer"
  [ testProperty "wrap" $ property $ do
      x <- forAll genInteger
      x === I.unwrap (I.wrap x)

  , tt_Integer'lr @(P 0)   @(P 0)
  , tt_Integer'lr @(P 0)   @(P 1)
  , tt_Integer'lr @(P 0)   @(P 100)
  , tt_Integer'l  @(P 0)

  , tt_Integer'lr @(P 1)   @(P 1)
  , tt_Integer'lr @(P 1)   @(P 100)
  , tt_Integer'l  @(P 1)

  , tt_Integer'lr @(P 10)  @(P 10)
  , tt_Integer'lr @(P 10)  @(P 100)
  , tt_Integer'l  @(P 10)
  ]

tt_Integer'lr
  :: forall (l :: KI.Integer) (r :: KI.Integer)
  .  I.Inhabited Integer ('Just l) ('Just r)
  => TestTree
tt_Integer'lr = testGroup ("Interval [" <> show l <> ", " <> show r <> "]")
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
        [ testProperty "pred" $ property $ do
            x <- forAll $ genIInteger @('Just l) @('Just r)
            case I.pred' x of
              Nothing -> x === l
              Just y -> do x /== l
                           I.unwrap y === I.unwrap x - 1

        , testProperty "succ" $ property $ do
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
      Nothing === I.negate' x

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


tt_Integer'l
  :: forall (l :: KI.Integer)
  .  I.Inhabited Integer ('Just l) 'Nothing
  => TestTree
tt_Integer'l = testGroup ("Interval [" <> show l <> ", infinity)")
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

  , pure $ testProperty "mult'" $ property $ do
      a <- forAll $ genIInteger @('Just l) @'Nothing
      b <- forAll $ genIInteger @('Just l) @'Nothing
      let x = toInteger (I.unwrap a) * toInteger (I.unwrap b)
      case I.mult' a b of
        Nothing -> assert (x < l'')
        Just y -> toInteger (I.unwrap y) === x


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

  , pure $ testProperty "pred" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      case I.pred' x of
        Nothing -> x === l
        Just y -> do x /== l
                     I.unwrap y === I.unwrap x - 1

  , pure $ testProperty "succ" $ property $ do
      x <- forAll $ genIInteger @('Just l) @'Nothing
      case I.succ' x of
        Nothing -> failure
        Just y -> do I.unwrap y === I.unwrap x + 1

  , case KI.cmpInteger (Proxy @l) (Proxy @(P 0)) of
      EQI -> pure $ testCase "zero" $
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

--------------------------------------------------------------------------------

genWord8 :: MonadGen m => m Word8
genWord8 = Gen.integral $ Range.constant minBound maxBound

genInt8 :: MonadGen m => m Int8
genInt8 = Gen.integral $ Range.constant minBound maxBound

genNatural :: MonadGen m => m Natural
genNatural = Gen.integral $ Range.linear 0 (10 ^ (100 :: Int))

genInteger :: MonadGen m => m Integer
genInteger = Gen.integral $ Range.linearFrom 0 (negate (10 ^ (100 :: Int)))
                                               (10 ^ (100 :: Int))

genRational :: MonadGen m => m Rational
genRational = do
  n <- genInteger
  d <- Gen.integral $ Range.linear 1 (10 ^ (100 :: Int))
  pure (n :% d)

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
  => Maybe (Dict (a L.<= b))
leInteger = case KI.cmpInteger (Proxy @a) (Proxy @b) of
  L.LTI -> Just $ unsafeCoerce (Dict @())
  L.EQI -> Just $ unsafeCoerce (Dict @())
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
integerMagic1 f =
  Sub $ unsafeCoerce (IntegerMagic Dict) $ f (KI.integerVal (Proxy @a))

negateInteger
  :: forall (a :: KI.Integer)
  .  (KI.KnownInteger a) :- (KI.KnownInteger (KI.Negate a))
negateInteger = integerMagic1 negate

