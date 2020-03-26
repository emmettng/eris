module Eris.Pantheon.Boltzmann where

import Eris.Meta.DataTypes
import qualified Data.Vector.Storable as DV

{- Other functions that being used to handle categorical representations -}
-- | Hamming Distance 
-- Calculate the number of positions at which corresponding elements are different.
hmi :: Vector -> Vector -> Double
hmi v1 v2 =
  let tmp1 = DV.zipWith (==) v1 v2
      tmp2 =
        DV.map
          (\x ->
              if x
                then 0
                else 1)
          tmp1
  in DV.foldr (+) 0 tmp2
