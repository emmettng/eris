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
      cosineDistance,
      cosineSimilarity,
      meanSquaredDistance,
      pearsonCC,
      pearsonCC',
      pairWiseSimilarity
    ) where
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Numeric.LinearAlgebra

import Eris.Meta.DataStructure


-- | L1-norm
sumAbsoluteDifference:: RankMetric
sumAbsoluteDifference l1 l2 = norm_1 (v1 - v2)
    where v1 = vector l1
          v2 = vector l2

-- | Mean absolute Error
meanAbsoluteDifference :: RankMetric
meanAbsoluteDifference l1 l2 = l1norm / d
    where l1norm = sumAbsoluteDifference l1 l2
          d = fromIntegral . length $ l1

-- | Manhattan Distance is l1 norm.
manhattanDistance :: RankMetric
manhattanDistance = sumAbsoluteDifference


-- | Squared L2-norm
sumSquaredDifference :: RankMetric
sumSquaredDifference l1 l2 = v <.> v
    where v = vector l1 - vector l2

-- | Mean Sqaured Error
meanSquaredDistance :: RankMetric
meanSquaredDistance l1 l2 = squaredL2 / d
    where squaredL2 = sumSquaredDifference l1 l2
          d = fromIntegral . length $ l1

euclideanDistance :: RankMetric
euclideanDistance l1 l2 = norm_2 v
    where v = vector l1 - vector l2

cosineSimilarity :: RankMetric
cosineSimilarity l1 l2 = v1 <.> v2 / (norm_2 v1 * norm_2 v2)
    where
        v1 = vector l1
        v2 = vector l2

cosineDistance :: RankMetric
cosineDistance l1 l2 = 1 - cosineSimilarity l1 l2

-- | Pearson Correlation Coefficient
-- It is cosine similarity between centered vectors.
-- reference to : https://stats.stackexchange.com/questions/235673/is-there-any-relationship-among-cosine-similarity-pearson-correlation-and-z-sc
pearsonCC :: RankMetric
pearsonCC l1 l2 = v1 <.> v2 / (norm_2 v1 * norm_2 v2)
    where len = fromIntegral . length $ l1
          tv1 = vector l1
          tv2 = vector l2
          m1 = sumElements tv1 / len
          m2 = sumElements tv2 / len
          v1 = tv1 - vector [m1]
          v2 = tv2 - vector [m2]

pearsonCC' :: RankMetric
pearsonCC' l1 l2 = z1 <.> z2 /d
    where z1 = zScore l1
          z2 = zScore l2
          d = fromIntegral . length $ l1

-- | a little bit involove actually
zScore :: [Rank] -> Vector Rank
zScore xs = scale (sqrt d / l2normV ) v
    where x = vector xs
          d = fromIntegral . length $ xs
          meanX = sumElements x / d
          v = x - vector [meanX]
          l2normV = norm_2 v


pairWiseSimilarity :: Threshold
                      -> SimilarityMatrix
                      -> RankMetric
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
