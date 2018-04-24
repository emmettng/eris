module Eris.Meta.DataStructure where

import qualified Data.Set as DS
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as B

type Threshold = Integer

type EID = B.ByteString

type Score = Double

type SimilarityMatrix = Map.HashMap EID (Map.HashMap EID Score)
type ESMap = Map.HashMap EID Double
type ECount = Map.HashMap EID ESMap
