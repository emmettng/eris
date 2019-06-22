{-# LANGUAGE DeriveGeneric #-}
module Eris.Meta.DataStructure where

import qualified Data.Set as DS
import qualified Data.HashMap.Strict as Map
-- import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Short as B
import GHC.Generics (Generic)
import Data.Csv

type Threshold = Integer


-- | EID : Entity ID
-- Entity represent either products or customers
-- It must be discete
type EID = B.ShortByteString
type CID = EID
type PID = EID
type Neighbors = [EID]
type NeighborSimilarity = [(EID,Double)]
type NeighborRank = [(EID,Rank)]

data Order = Desc | Asc deriving (Eq)

-- | Relation between two kinds of entities
-- e.g:
--    1. Similarity between customers
--    2. Similarity between productes
--    3. Customers' rating of products.
type Rank = Double

-- | 2D matrix of Entity type A and Entity type B ( A & B could be the same type).
-- Map.lookupDefault for querying this matrix.
-- Represent:
--    0. Matrix of customers' rating of product, outter key is group by certain dimension (Entity type)
-- ESMap is alias for convience.
type ECount = Map.HashMap EID ESMap
type ESMap = Map.HashMap EID Double

-- | Alias of Ecount
-- Map.lookupDefault for querying this matrix.
-- Represent:
--    0. Similarity Dictionary between entities of the same type.
--  :: Map.HashMap EID (Map.HashMap EID Rank)
type SimilarityMatrix = ECount



type GroupName = String
type RankMetric = [Rank] -> [Rank] -> Rank


-- | SaleRecord to be able to parse by Data.Csv
data SaleRecord =  SaleRecord { sku::EID , cid::EID, cRanking::Double} deriving  (Generic, Show)

instance FromNamedRecord SaleRecord
instance ToNamedRecord SaleRecord
instance DefaultOrdered SaleRecord
instance FromRecord SaleRecord
