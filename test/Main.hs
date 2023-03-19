{-# LANGUAGE UndecidableSuperClasses #-}
module Main (main) where

import Control.Monad
import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Word
import Data.Type.Ord
import GHC.Real (Ratio((:%)))
import GHC.TypeNats
import Hedgehog (MonadGen, forAll, property, (===))
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
  [ tt_Natural
  ]

tt_Natural :: TestTree
tt_Natural = testGroup "Natural"
  [
  ]

--------------------------------------------------------------------------------

class GenI x where
  genI :: forall l r m. (MonadGen m, I.Shove x l r) => m (I x l r)

instance {-# OVERLAPPABLE #-} (Integral x, Bounded x) => GenI x where
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

