{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module is designed to be imported as follows:
--
-- @
-- import "I" ('I')
-- import "I" qualified
-- @
module I
 ( -- * I
   I
 , T
 , MinT
 , MaxT
 , L
 , MinL
 , R
 , MaxR
 , Interval(..)
 , unwrap
 , wrap
 , unsafe
 , Clamp(..)
 , Up
 , up
 , down
 , Discrete(..)
 , Succ(..)
 , Pred(..)
 , One(..)
 , Zero(..)
 , Negate(..)
 , Plus(..)
 , Mult(..)
 , Minus(..)
 , Div(..)
   -- * Known
 , Known(..)
 , known
 , With(..)
 , min
 , max
 , single
   -- * Testing support
 , Shove(..)
   -- * Danger zone
 , unsafest
 ) where

import I.Internal
import I.Int8 ()
import I.Word8 ()
import I.Natural ()
import I.Integer ()
import I.Rational ()

import I.Autogen.CChar ()
import I.Autogen.CInt ()
import I.Autogen.CIntMax ()
import I.Autogen.CIntPtr ()
import I.Autogen.CLLong ()
import I.Autogen.CLong ()
import I.Autogen.CPtrdiff ()
import I.Autogen.CSChar ()
import I.Autogen.CShort ()
import I.Autogen.CSize ()
import I.Autogen.CUChar ()
import I.Autogen.CUInt ()
import I.Autogen.CUIntMax ()
import I.Autogen.CUIntPtr ()
import I.Autogen.CULLong ()
import I.Autogen.CULong ()
import I.Autogen.CUShort ()
import I.Autogen.CWchar ()
import I.Autogen.Int ()
import I.Autogen.Int16 ()
import I.Autogen.Int32 ()
import I.Autogen.Int64 ()
import I.Autogen.Word ()
import I.Autogen.Word16 ()
import I.Autogen.Word32 ()
import I.Autogen.Word64 ()

