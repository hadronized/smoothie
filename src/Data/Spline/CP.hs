-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Data.Spline.CP (
    -- * Control points
    CP(..)
  ) where

-- | A 'CP' is a *control point*. A curve passes through control points and
-- the shape of the curve is determined by the polynomials used to interpolate
-- values in between.
--
-- @CP s a@ is a control point of sampling type 's' and carried type 'a'. In
-- most cases, 's' must be 'Ord' and 'a' must be 'Additive' and 'Fractional'.
data CP s a = CP !s !a deriving (Functor,Eq,Ord,Show)
