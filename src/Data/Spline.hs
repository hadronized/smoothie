-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- A @Spline s a@ represents a curve in which 'a' is very likely to be
-- 'Additive' (see "linear") and 's' is the sampling type.
--
-- The library exports two useful functions: 'spline' and 'smooth'. The former
-- enables you to create splines while the latter enables you to sample from
-- them using their control points.
----------------------------------------------------------------------------

module Data.Spline (
    module X
  ) where

import Data.Spline.Curve as X
