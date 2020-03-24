module Eris.Distance.Minkowski where

-- import Eris.Meta.DataTypes
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

{-
   This module contains different derivation of Minkowski distance. 
   These functions define distance between two high dimensional vectors X and Y.
   One distance definition may have many different names .

   1. sad (sub absolute difference, taxicab metric, manhattan distance, l1-norm of (X-Y) ) 
-}
-- | sad 
-- sad :: Vector -> Vector -> Double
-- sad v1 v2 = norm_1 (v1 - v2)
sad' :: Vector Double -> Vector Double -> Double
sad' v1 v2 = norm_1 $ v1 - v2
