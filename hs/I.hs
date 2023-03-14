{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module is designed to be imported as follows:
--
-- @
-- import "I" ('I')
-- import "I" qualified
-- @
module I
 ( -- * Intervals
   I
 , unwrap
   -- * Type-level elements
 , T
 , MinT
 , MaxT
   -- * Left end
 , L
 , MinL
   -- * Right end
 , R
 , MaxR
   -- * Known intervals
 , Interval(..)
 , Inhabited(..)
 , wrap
 , clamp
 , Pred(..)
 , Succ(..)
 , One(..)
 , Zero(..)
 , Plus(..)
 , PlusInv(..)
 , Minus(..)
 , Mult(..)
 , MultInv(..)
 , div
   -- * Known interval elements
 , Known(..)
 , With(..)
 , min
 , max
 , single
 , KnownPred(..)
 , KnownSucc(..)
 ) where

import I.Internal
import I.Int8 ()
import I.Word8 ()

-- Generated by `sh ./genmodules.sh`
import I.Generated.CChar ()
import I.Generated.CInt ()
import I.Generated.CIntMax ()
import I.Generated.CIntPtr ()
import I.Generated.CLLong ()
import I.Generated.CLong ()
import I.Generated.CPtrdiff ()
import I.Generated.CSChar ()
import I.Generated.CShort ()
import I.Generated.CSize ()
import I.Generated.CUChar ()
import I.Generated.CUInt ()
import I.Generated.CUIntMax ()
import I.Generated.CUIntPtr ()
import I.Generated.CULLong ()
import I.Generated.CULong ()
import I.Generated.CUShort ()
import I.Generated.CWchar ()
import I.Generated.Int ()
import I.Generated.Int16 ()
import I.Generated.Int32 ()
import I.Generated.Int64 ()
import I.Generated.Word ()
import I.Generated.Word16 ()
import I.Generated.Word32 ()
import I.Generated.Word64 ()

