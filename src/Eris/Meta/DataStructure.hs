module Eris.Meta.DataStructure where

import qualified Data.Set as DS
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as B

type Threshold = Integer

type EID = B.ByteString
type TEID = EID
type IEID = EID

type Score = Double

type ESMap = Map.HashMap TEID Double
type ECount = Map.HashMap IEID ESMap
