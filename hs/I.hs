{-# LANGUAGE NoImplicitPrelude #-}

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
import I.Word8 ()
import I.Word16 ()
import I.Word32 ()
import I.Word64 ()

