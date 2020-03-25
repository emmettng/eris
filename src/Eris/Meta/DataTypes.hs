module Eris.Meta.DataTypes where

import qualified Data.Vector.Storable as V

-- | Functions in hMatrix involves manipulating Vector and Matrix. 
-- Corresponding definitions of Vector and Matrix  relies on Data.Vector.Storable.
-- https://hackage.haskell.org/package/hmatrix-0.20.0.0/docs/Numeric-LinearAlgebra-Data.html#t:Vector
--
-- In this package, Vector is not as complicate as in hMatrix. 
-- Vector is specified as containing elements of the type "Double".
--
type Vector = V.Vector Double
