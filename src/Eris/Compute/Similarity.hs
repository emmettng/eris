module Eris.Compute.Similarity
    (
    ) where
import qualified Data.HashMap.Strict as Map
import Data.Maybe

import Eris.Meta.DataStructure
consine :: Threshold -> ECount -> SimilarityMatrix
consine th ecount = let
            target = head . Map.toList $ ecount
            items = tail . Map.toList $ ecount
            similarVector = fmap (consineAuxil target) items
          in composeMatrix target items similarVector
          where
                consineAuxil :: (EID, ESMap)-> (EID,ESMap)-> Double
                consineAuxil (_, tMap) (_,iMap) =
                  discreteConsine tMap iMap
                discreteConsine :: ESMap -> ESMap -> Double
                discreteConsine t i = let
                    intersect = Map.toList $ Map.intersection t i
                    tv = [ snd v |v <- intersect]
                    iv = catMaybes [ Map.lookup (fst v) i| v <- intersect]
                    dot a b = sum $ zipWith (*) a b
                    elen a = sqrt $ dot a a
                    in
                      if null tv
                       then 0
                      else
                        dot tv iv / (elen tv * elen iv)
                composeMatrix :: (EID, ESMap) -> [(EID, ESMap)] -> [Double] -> SimilarityMatrix
                composeMatrix tt ilist = let
                    idList = [fst v | v <- ilist]
                    tid = fst tt
                    in undefined
