{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

-- | "I" am a Haskell module designed to be imported as follows:
--
-- @
-- import "I" ('I')
-- import "I" qualified
-- @
--
-- 'I' exist so that you don't have to manually check that a value is within
-- an interval. For example:
--
-- [@'I' 'Prelude.Int' ('KindInteger.N' 5) ('KindInteger.P' 5)@]
-- An 'Prelude.Int' known to be in the interval /[-5, +5]/.
--
-- [@'I' 'Numeric.Natural.Natural' 100 ''Prelude.Nothing'@]
-- A 'Numeric.Natural.Natural' known to be in the interval /[100, +infinity)/.
--
-- [@'I' 'Prelude.Rational' (''Prelude.Just' '( ''Prelude.False', 0 'KindRational./' 1)) (''Prelude.Just' '( ''Prelude.True', 1 'KindRational./' 2))@]
-- A 'Prelude.Rational' known to be in the interval /(0, +0.5]/.
module I
 ( -- * Interval
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
 , Up(..)
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
   -- * Testing
 , Shove(..)
   -- * Danger
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

