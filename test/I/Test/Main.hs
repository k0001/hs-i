{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module I.Test.Main (main) where

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

import I.Test.Int8 qualified
import I.Test.Integer qualified
import I.Test.Natural qualified
import I.Test.Support
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
  ]

