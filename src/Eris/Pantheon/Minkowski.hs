module Eris.Pantheon.Minkowski where

import Eris.Meta.DataTypes
import Numeric.LinearAlgebra hiding (Vector)
import qualified Data.Vector.Storable as DV

-- import Numeric.LinearAlgebra.Data
{-
   This module contains different derivation of Minkowski distance. 
   These functions define distance between two high dimensional vectors X and Y.
   One distance definition may have many different names .

   1. sad (sub absolute difference, taxicab metric, manhattan distance, l1-norm of (X-Y) ) 
-}
{- L1 norm family -}
-- Sum of Absolute Difference: norm(l1) of (v1 - v2)
-- sad , taxicab, manhattan distance are different names of the same implementation.
sad :: Vector -> Vector -> Double
sad v1 v2 = norm_1 (v1 - v2)

taxicab :: Vector -> Vector -> Double
taxicab = sad

manhattanDistance :: Vector -> Vector -> Double
manhattanDistance = sad

-- | Mean Absolute Difference 
mad :: Vector -> Vector -> Double
mad v1 v2 = norm1 / n
  where
    norm1 = sad v1 v2
    n = fromIntegral . DV.length $ v1

{- L2 norm based -}
-- | Euclidean Distance 
--
euclideanDistance :: Vector -> Vector -> Double
euclideanDistance v1 v2 = norm_2 (v1 - v2)

-- | (L2 norm based) Sum Suqred Difference
-- Squred euclidean distance
ssd :: Vector -> Vector -> Double
ssd v1 v2 = v <.> v
  where
    v = v1 - v2

-- | (L2 norm based) Mean Sqaured Error (MSE)
--  ssd averaged over the vector
mse :: Vector -> Vector -> Double
mse v1 v2 = squaredL2 / n
  where
    squaredL2 = ssd v1 v2
    n = fromIntegral . DV.length $ v1

-- | (L2 norm based) Root mean squared Error (rmse)
-- euclidean distance averged over the vector
rmse :: Vector -> Vector -> Double
rmse v1 v2 = euclideanDistance v1 v2 / sqrtN
  where
    sqrtN = sqrt . fromIntegral $ DV.length v1

-- | Minkowski Distance
-- This is a degenerate case of Minkowski distance 
-- in this case p is an Integral number for sure.
minkowskiDistance :: Int -> Vector -> Vector -> Double
minkowskiDistance p v1 v2 =
  let vDiff = v1 - v2
      absV = DV.map abs vDiff
  in lpnorm p absV
  where
    lpnorm :: Int -> Vector -> Double
    lpnorm 0 vec =
      let vl = -DV.length vec
      in norm_1 $ DV.map (f0 vl) vec
    lpnorm p vec = nroot p $ norm_1 $ DV.map (\n -> n ^ p) vec
    nroot
      :: (Integral a, Floating b)
      => a -> b -> b
    nroot 0 _ = 1
    nroot n f = f ** (1 / fromIntegral n)
    f0 :: Int -> Double -> Double
    f0 l v = (2 ^ l) * (v / (1 + v))

{- Minkowski based (p = 0, p -> infinity) -}
-- | Chebyshev Distance 
-- The limiting case of Minkowski distance when p reaches infinity 
cbv :: Vector -> Vector -> Double
cbv v1 v2 =
  let absV = DV.map abs $ v1 - v2
  in DV.maximum absV

-- | Canberra Distance 
--
cbr :: Vector -> Vector -> Double
cbr v1 v2 =
  let sum1 = DV.sum . (DV.map abs) $ v1
      sum2 = DV.sum . (DV.map abs) $ v2
  in (sad v1 v2) / (sum1 + sum2)
