-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This package works around two primary types:
--
--   - 'Key'
--   - 'Spline'
--
-- A @'Spline' a s@ represents a curve in which 'a' is very likely to be
-- 'Additive' (see <https://hackage.haskell.org/package/linear linear>) and
-- 's' is the sampling type.
--
-- A 'Key' is used to hold data in a 'Spline'. It adds interpolation mode to
-- data for __each__ 'Key' used to build the 'Spline'.
--
-- Through the library, you’ll see types like:
--
-- @ ('Additive' a) => a s @
--
-- That is due to the fact some functions work on 'a' as a polymorphic
-- first-class value. That enables more flexibility in the implemantation and
-- the interface. Thus, in most cases, you can any type of your choice as long
-- as it’s an aditive one.
----------------------------------------------------------------------------

module Data.Spline (
    -- * Re-exports
    module Data.Spline.Curve
  ) where

import Data.Spline.Curve
