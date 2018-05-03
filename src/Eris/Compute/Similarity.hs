-- |
-- Module         : Eris.Compute.Similarity
-- Copyright      : (c) 2018 Emmett H. Ng
-- License        : BSD3
--
-- Maintainer     : emmettng@gmail.com
-- Stability      : experimental & educational
-- Portability    : portable
--
-- Functions of computing pairwise similarity and similarity matrix.

module Eris.Compute.Similarity
    (
      cosineSimilarity,
      msdSimilarity,
      meanSimilarity,
      pairWiseSimilarity
    ) where
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Numeric.LinearAlgebra

import Eris.Meta.DataStructure


cosineSimilarity :: RankDistance
cosineSimilarity l1 l2 = v1 <.> v2 / (norm_2 v1 * norm_2 v2)
    where
        v1 = vector l1
        v2 = vector l2

msdSimilarity :: RankDistance
msdSimilarity l1 l2 =  norm_2 (v1 - v2) / len
    where v1 = vector l1
          v2 = vector l2
          len = fromIntegral . length $ l1

meanSimilarity :: RankDistance
meanSimilarity l1 l2 = vm1 <.> vm2 / (norm_2 vm1 * norm_2 vm2)
    where len = fromIntegral . length $ l1
          v1 = vector l1
          v2 = vector l2
          m1 = norm_1 v1 / len
          m2 = norm_1 v2 / len
          vm1 = v1 - vector [m1]
          vm2 = v2 - vector [m2]

pairWiseSimilarity :: Threshold
                      -> SimilarityMatrix
                      -> RankDistance
                      -> ECount
                      -> SimilarityMatrix
pairWiseSimilarity bar cumSM sfn ecount=
          let
            target = head . Map.toList $ ecount
            items = tail . Map.toList $ ecount
            sVector = pairwiseAuxil target <$> items
            sMatrix = composeMatrix target items sVector
          in
            if null items
              then Map.union cumSM sMatrix
            else
              pairWiseSimilarity bar (Map.union cumSM sMatrix) sfn (Map.fromList items)
          where
                pairwiseAuxil :: (EID, ESMap)-> (EID,ESMap)-> Double
                pairwiseAuxil (_, tMap) (_,iMap) =
                  esmapSFN tMap iMap
                esmapSFN :: ESMap -> ESMap -> Double
                esmapSFN t i = let
                    intersect = Map.toList $ Map.intersection t i
                    tv = [ snd v |v <- intersect]
                    iv = catMaybes [ Map.lookup (fst v) i| v <- intersect]
                    in
                      if null intersect
                       then 0
                      else
                        sfn tv iv
                composeMatrix :: (EID, ESMap) -> [(EID, ESMap)] -> [Double] -> SimilarityMatrix
                composeMatrix tt ilist slist = let
                    tid = fst tt
                    idList = [fst v | v <- ilist]
                    tidSelf = Map.fromList [(tid,1)]
                    tmpDict = Map.union tidSelf $ Map.fromList $ zip idList slist
                    in Map.fromList [(tid,tmpDict)]
