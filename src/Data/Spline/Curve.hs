{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module defines 'Spline's and related functions. Because a 'Spline'
-- requires 'Key's, the "Data.Spline.Key" module is also exported.
-----------------------------------------------------------------------------

module Data.Spline.Curve (
    -- * Splines
    Spline
  , splineKeys
    -- * Building splines
  , spline
    -- * Sampling splines
  , sample
    -- * Re-exported
  , module Data.Spline.Key
  ) where

import Control.Monad ( guard )
import Data.Aeson
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Spline.Key
import Data.Vector ( Vector, (!?), fromList )
import Linear ( Additive )

-- |A @'Spline' a s@ is a collection of 'Key's with associated interpolation
-- modes.
--
-- Given two 'Key's which indices are /i/ and /i+1/, the interpolation mode on
-- the resulting curve is performed using the interpolation mode of the key /i/.
-- Thus, the interpolation mode of the latest key might be ignored. There’s an
-- exception, though, when using the 'Bezier' interpolation mode. Feel free
-- to dig in the "Key" documentation.
newtype Spline a s = Spline {
    -- |Extract the 'Key's.
    splineKeys :: Vector (Key (a s))
  } deriving (Eq,Functor,Show)

instance (FromJSON (a s), Ord s) => FromJSON ((a s -> s) -> Spline a s) where
  parseJSON value = do
    keys <- parseJSON value
    pure $ \sampler -> spline sampler keys

instance (ToJSON (a s)) => ToJSON (Spline a s) where
  toJSON = Array . fmap toJSON . splineKeys

-- |Build a @'Spline' a s@.
--
-- @a s@ is the type held by 'Key's. For instance, @'V2' Float@.
--
-- The first argument of the function, which has type @a s -> s@, is a function
-- used to extract the sampling value of each 'Key's. In most cases, that value
-- represents the time or the frame of a simulation. That value is used to
-- perform sampling comparison.
spline :: (Ord s)
       => (a s -> s)
       -> [Key (a s)]
       -> Spline a s
spline sampler = Spline . fromList . sortBy (comparing $ sampler . keyValue)

-- |Sample a 'Spline' at a given 's' sampling value. If no sample exists,
-- yields 'Nothing'.
--
-- The first parameter is a /sampler/ function used to extract a comparison
-- value. For most curves, the reflected value should be the time or the frame.
sample :: (Additive a,Floating s,Ord s)
       => (a s -> s)
       -> Spline a s
       -> s
       -> Maybe (a s)
sample sampler (Spline keys) at = do
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
