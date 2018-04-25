module Eris.Compute.Similarity
    (
      consine 
    ) where
import qualified Data.HashMap.Strict as Map
import Data.Maybe

import Eris.Meta.DataStructure

consine :: [Double] -> [Double] -> Double
consine v1 v2 = dot v1 v2 / (elen v1 * elen v2)
    where
        dot a b = sum $ zipWith (*) a b
        elen a = sqrt $ dot a a

consineSimilarity :: Threshold -> ECount -> SimilarityMatrix
consineSimilarity th ecount = let
            target = head . Map.toList $ ecount
            items = tail . Map.toList $ ecount
            similarVector = fmap (consineAuxil target) items
          in composeMatrix target items similarVector
          where
                consineAuxil :: (EID, ESMap)-> (EID,ESMap)-> Double
                consineAuxil (_, tMap) (_,iMap) =
                  esmapConsine tMap iMap
                esmapConsine :: ESMap -> ESMap -> Double
                esmapConsine t i = let
                    intersect = Map.toList $ Map.intersection t i
                    tv = [ snd v |v <- intersect]
                    iv = catMaybes [ Map.lookup (fst v) i| v <- intersect]
                    dot a b = sum $ zipWith (*) a b
                    elen a = sqrt $ dot a a
                    in
                      if null intersect
                       then 0
                      else
                        dot tv iv / (elen tv * elen iv)
                composeMatrix :: (EID, ESMap) -> [(EID, ESMap)] -> [Double] -> SimilarityMatrix
                composeMatrix tt ilist slist = let
                    tid = fst tt
                    idList = [fst v | v <- ilist]
                    tidSelf = Map.fromList [(tid,1)]
                    tmpDict = Map.union tidSelf $ Map.fromList $ zip idList slist
                    in Map.fromList [(tid,tmpDict)]
