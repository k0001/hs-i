module Main (main) where

import Data.Word
import Data.Proxy

import I (I)
import I qualified

main :: IO ()
main = pure ()

_test_with_identity_typechecks :: I Word8 33 77
_test_with_identity_typechecks =
  let x = I.inhabitant `asTypeOf` _test_with_identity_typechecks
  in  I.with x (\(_ :: Proxy b) -> I.known @_ @b)

