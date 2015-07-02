{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Data.Spline.Curve (
    -- * Spline
    Spline
  , splineKeys
  , splineSampler
    -- * Building splines
  , spline
    -- * Sampling splines
  , sample
    -- * Re-exported
  , module X
  ) where

import Control.Monad ( guard )
import Data.Aeson
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Spline.Key as X
import Data.Vector ( Vector, (!?), fromList )
import Linear ( Additive )

-- |A @Spline@ is a collection of keys with associated interpolation modes.
-- Given two keys which indices are /i/ and /i+1/, the interpolation mode on the
-- resulting curve is performed using the interpolation mode of the key /i/.
-- Thus, the interpolation mode of the latest key might be ignored. There’s an
-- exception, though, when using the 'Bezier' interpolation mode.
data Spline a s = Spline {
    -- |Extract the keys.
    splineKeys :: Vector (Key (a s))
    -- |Extract the sampler.
  , splineSampler :: a s -> s
  }

instance (FromJSON (a s), Ord s) => FromJSON ((a s -> s) -> Spline a s) where
  parseJSON = withObject "spline" $ \o -> do
    keys <- o .: "keys"
    pure $ \sampler -> spline sampler keys

-- |Build a 'Spline a s'.
--
-- 'a s' is the type hold by keys. For instance, @V2 Float@.
--
-- The first argument of the function, which has type @a s -> s@ is a function
-- used to extract the sampling value of each keys. In most cases, that value
-- represents the time or the frame of a simulation. That value is used to
-- perform sampling comparison.
spline :: (Ord s)
       => (a s -> s)
       -> [Key (a s)]
       -> Spline a s
spline sampler keys =
  Spline (fromList $ sortBy (comparing $ sampler . keyValue) keys) sampler

-- |Sample a 'Spline' at a given 's' sampling value. If no sample exists,
-- yields 'Nothing'.
sample :: (Additive a,Floating s,Ord s) => Spline a s -> s -> Maybe (a s)
sample (Spline keys sampler) at = do
  i <- bsearchLower (\k -> compare at (sampler $ keyValue k)) keys
  k0 <- keys !? i
  k1 <- keys !? (i + 1)
  pure $ interpolateKeys (normalizeSampling sampler at k0 k1) k0 k1

-- Helper binary search that searches the ceiling index for the
-- value to be searched according to the predicate.
bsearchLower :: (a -> Ordering) -> Vector a -> Maybe Int
bsearchLower p v = go 0 (length v - 1)
  where
    go start end = do
        guard (start <= end)
        ma <- v !? m
        ma1 <- v !? succ m
        case p ma of
          LT -> go start (pred m)
          EQ -> Just m
          GT -> if p ma1 == LT then Just m else go (succ m) end
      where
        m = (end + start) `div` 2
