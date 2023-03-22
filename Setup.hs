{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.List (stripPrefix)
import Distribution.Compat.Time (getModTime)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..))
import Distribution.Simple.BuildPaths (autogenComponentModulesDir, getSourceFiles)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withLibLBI)
import Distribution.Simple.Utils (debug)
import Distribution.Verbosity (normal)
import System.Directory (createDirectoryIfMissing, doesFileExist)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { buildHook = \pd lbi uh bf -> do
      generate pd lbi
      buildHook simpleUserHooks pd lbi uh bf
  , haddockHook = \pd lbi uh bf -> do
      generate pd lbi
      haddockHook simpleUserHooks pd lbi uh bf
  , replHook = \pd lbi uh bf args -> do
      generate pd lbi
      replHook simpleUserHooks pd lbi uh bf args
  }
  where
    generate :: PackageDescription -> LocalBuildInfo ->  IO ()
    generate pd lbi = do
      withLibLBI pd lbi $ \lib clbi -> do
        lMods <- getSourceFiles normal ["hs"] ["I.Word8", "I.Int8"]
        Just mWord8Path <- pure $ lookup "I.Word8" lMods
        Just mInt8Path  <- pure $ lookup "I.Int8"  lMods

        let lAutogenDir = autogenComponentModulesDir lbi clbi
            lIAutogenDir = lAutogenDir <> "/I/Autogen"
        createDirectoryIfMissing True lIAutogenDir

        mWord8Source <- readFile mWord8Path
        mWord8ModTime <- getModTime mWord8Path
        forM_ likeWord8 $ \like -> do
          let likePath = lIAutogenDir <> "/" <> like <> ".hs"
          gen <- doesFileExist likePath >>= \case
            False -> pure True
            True  -> do likeModTime <- getModTime likePath
                        pure (likeModTime < mWord8ModTime)
          when gen $ do
            writeFile likePath
              $ replace "Word8" like
              $ replace "module I.Word8" "module I.Autogen.Word8"
              $ mWord8Source
            debug normal ("Generated " <> likePath)

        mInt8Source <- readFile mInt8Path
        mInt8ModTime <- getModTime mInt8Path
        forM_ likeInt8 $ \like -> do
          let likePath = lIAutogenDir <> "/" <> like <> ".hs"
          gen <- doesFileExist likePath >>= \case
            False -> pure True
            True  -> do likeModTime <- getModTime likePath
                        pure (likeModTime < mInt8ModTime)
          when gen $ do
            writeFile likePath
              $ replace "Int8" like
              $ replace "module I.Int8" "module I.Autogen.Int8"
              $ mInt8Source
            debug normal ("Generated " <> likePath)


likeWord8 :: [String]
likeWord8 =
  [ "Word"
  , "Word16"
  , "Word32"
  , "Word64"
  , "CUChar"
  , "CUShort"
  , "CUInt"
  , "CULong"
  , "CSize"
  , "CULLong"
  , "CUIntPtr"
  , "CUIntMax"
  ]

likeInt8 :: [String]
likeInt8 =
  [ "Int"
  , "Int16"
  , "Int32"
  , "Int64"
  , "CChar"
  , "CSChar"
  , "CShort"
  , "CInt"
  , "CLong"
  , "CPtrdiff"
  , "CWchar"
  , "CLLong"
  , "CIntPtr"
  , "CIntMax"
  ]

replace
  :: String  -- ^ Old substring.
  -> String  -- ^ New substring.
  -> String  -- ^ Old string.
  -> String  -- ^ New string.
replace o n [] = []
replace o n x@(x0:xs)
  | Just y <- stripPrefix o x = n <> replace o n y
  | otherwise = x0 : replace o n xs


