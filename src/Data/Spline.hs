-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- A @Spline a s@ represents a curve in which 'a' is very likely to be
-- 'Additive' (see "linear") and 's' is the sampling type.
----------------------------------------------------------------------------

module Data.Spline (
    module X
  ) where

import Data.Spline.Curve as X
