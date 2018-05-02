{-# LANGUAGE DeriveGeneric #-}
module Eris.Meta.DataStructure where

import qualified Data.Set as DS
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics (Generic)
import Data.Csv

type Threshold = Integer

-- | Element ID
-- Each Eelement is has an unique ID, and based upon different measurements of inviduls of the same type, the difference could be identified with different dimensions.

type EID = B.ByteString

-- | Being use to denote relation between two elements upon certain dimension.
-- This relation could be similarity or other, such as rating.
type Score = Double

--  | Pair-wise similarity of a certain dimension.
type SimilarityMatrix = Map.HashMap EID (Map.HashMap EID Score)

-- | Simalrity Dictionary, the outter key is group by dimension.
-- ESMap is alias for convience.
type ECount = Map.HashMap EID ESMap
type ESMap = Map.HashMap EID Double

type GroupName = String
type VectorDistance = [Double] -> [Double] -> Double


-- | SaleRecord to be able to parse by Data.Csv
data SaleRecord =  SaleRecord { sku::B.ByteString, cid::B.ByteString, cRanking::Int} deriving  (Generic, Show)
instance FromNamedRecord SaleRecord
instance ToNamedRecord SaleRecord
instance DefaultOrdered SaleRecord
instance FromRecord SaleRecord
