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

module Data.Spline.Polynomial (
    -- * Polynomial
    Polynomial(unPolynomial)
    -- * Polynomials for interpolation
  , hold
  , linear
    -- * Helpers
  , bsearchLower
  ) where

import Control.Monad ( guard )
import Data.Spline.CP
import Data.Vector as V ( Vector, (!?), length )
import Linear ( Additive(lerp) )

-- |A 'Polynomial' is used to interpolate in between a spline’s control points.
newtype Polynomial s a = Polynomial { unPolynomial :: s -> Vector (CP s a) -> Maybe a}

-- |Constant polynomial – a.k.a. /no interpolation/.
--
-- Given two control points and a sample value in between, the 'hold' polynomial
-- won’t perform any interpolation but it just /holds/ the value carried by the
-- lower control point along the whole curve between the two control points.
hold :: (Ord s) => Polynomial s a
hold = Polynomial go
  where
    go s cps = do
        li <- bsearchLower (\(CP s' _) -> compare s s') cps
        CP _ r <- cps !? li
        return r

-- |Parametric linear polynomial.
--
-- This form applies a pre-filter on the input before performing a linear
-- interpolation. Instead of:
--
-- @ lerp x a b @
--
-- We have:
--
-- @ lerp (pref x) a b @
--
-- This can be used to implement 1-degree splines if @pref = id@, basic cubic
-- non-hermitian splines if @pref = (^3)@, cosine splines if
-- @pref = \x -> (1 - cos (x*pi)) * 0.5@, and so on and so forth.
linearBy :: (Additive a,Fractional s,Ord s) => (s -> s) -> Polynomial s (a s)
linearBy pref = Polynomial go
  where
    go s cps = do
        li <- bsearchLower (\(CP s' _) -> compare s s') cps
        lower <- cps !? li
        upper <- cps !? succ li
        return $ lerp_ s lower upper
    lerp_ x (CP s0 a) (CP s1 b) = lerp x' b a
      where
        x' = (pref x - s0) / (s1 - s0)

-- |1-degree polynomial – a.k.a. /straight line interpolation/, or /linear
-- interpolation/.
--
-- This polynomial connects control points with straight lines.
--
-- Note: implemented with @linearBy id@.
linear :: (Additive a,Fractional s,Ord s) => Polynomial s (a s)
linear = linearBy id

-- |Helper binary search that search the ceiling index for the
-- value to be searched according to the predicate.
bsearchLower :: (a -> Ordering) -> Vector a -> Maybe Int
bsearchLower p v = go 0 (pred $ V.length v)
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