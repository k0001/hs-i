{-# LANGUAGE AllowAmbiguousTypes #-}

module I.Test.Main (main) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(..))
import Test.Tasty.Runners qualified as Tasty

import I.Test.Int8 qualified
import I.Test.Integer qualified
import I.Test.Natural qualified
import I.Test.Rational qualified
import I.Test.Word8 qualified

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter, Tasty.listingTests ]
  $ Tasty.localOption (HedgehogTestLimit (Just 1000))
  $ tt

tt :: TestTree
tt = testGroup "I"
  [ I.Test.Word8.tt
  , I.Test.Int8.tt
  , I.Test.Natural.tt
  , I.Test.Integer.tt
  , I.Test.Rational.tt
  ]

