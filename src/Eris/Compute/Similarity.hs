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
-- Documents:
-- 1. https://numerics.mathdotnet.com/Distance.html 

module Eris.Compute.Similarity
    (
      sumAbsoluteDifference,
      meanAbsoluteDifference,
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




-- | Manhattan Distance is Absolute Value Norm(L1) of (v1-v2)
manhattanDistance :: RankMetric
manhattanDistance = sumAbsoluteDifference

-- | Taxicab metric is Absolute Value Norm (L1) of (v1 - v2)
taxicab :: RankMetric
taxicab = sumAbsoluteDifference

-- | SAD is Absolute Value Norm(L1) of (v1 - v2)
sumAbsoluteDifference:: RankMetric
sumAbsoluteDifference l1 l2 = norm_1 (v1 - v2)
    where v1 = vector l1
          v2 = vector l2

-- | Mean absolute Difference (MAD)
-- | SAD average on all dimensions
meanAbsoluteDifference :: RankMetric
meanAbsoluteDifference l1 l2 = l1norm / n
    where l1norm = sumAbsoluteDifference l1 l2
          n = fromIntegral . length $ l1




-- | L2-normal           
euclideanDistance :: RankMetric
euclideanDistance l1 l2 = norm_2 v
    where v = vector l1 - vector l2

-- | Squared L2-norm (SSD)
sumSquaredDifference :: RankMetric
sumSquaredDifference l1 l2 = v <.> v
    where v = vector l1 - vector l2

-- | Mean Sqaured Error (MSE)
meanSquaredDistance :: RankMetric
meanSquaredDistance l1 l2 = squaredL2 / n
    where squaredL2 = sumSquaredDifference l1 l2
          n = fromIntegral . length $ l1




-- | Measure of similairty between two NON-ZERO vectors
-- See note/similarities.md for more information.
-- cosine similarity and cosine distance are not metric function.
cosineSimilarity :: RankMetric
cosineSimilarity l1 l2 = v1 <.> v2 / (norm_2 v1 * norm_2 v2)
    where
        v1 = vector l1
        v2 = vector l2

-- | Cosine Distance is not metric function 
-- as it does not have the triangle inequality property
-- see angular similarity and angular distance (valid metric function) below.
cosineDistance :: RankMetric
cosineDistance l1 l2 = 1 - cosineSimilarity l1 l2


-- | angular distance represent the radians between two vector
-- it satisfies the triangle inequality 
-- in a recommendation system, it is assumed that all vectors are  positive.
angularDistance :: RankMetric 
angularDistance l1 l2 = 
    let sim = cosineSimilarity l1 l2 
        inverSim = acos sim 
    in 2 * inverSim / pi

angularSimilarity :: RankMetric
angularSimilarity l1 l2 = 1 - angularSimilarity l1 l2 

-- quickcheckout doc
-- pearsonCC and zscore relation doc



-- | Pearson Correlation Coefficient
-- It is cosine similarity between centered vectors.
-- reference to : https://stats.stackexchange.com/questions/235673/is-there-any-relationship-among-cosine-similarity-pearson-correlation-and-z-sc
-- necessity of centering : https://www.theanalysisfactor.com/center-on-the-mean/
pearsonCC :: RankMetric
pearsonCC l1 l2 = v1 <.> v2 / (norm_2 v1 * norm_2 v2)
    where len = fromIntegral . length $ l1
          tv1 = vector l1
          tv2 = vector l2
          m1 = sumElements tv1 / len
          m2 = sumElements tv2 / len
          v1 = tv1 - vector [m1]
          v2 = tv2 - vector [m2]

-- | Test shows that this implementation is supresingly better than above.
pearsonCC' :: RankMetric
pearsonCC' l1 l2 = z1 <.> z2 /d
    where z1 = zScore l1
          z2 = zScore l2
          d = fromIntegral . length $ l1

-- | a little bit involove actuallec
-- n : vector 
-- std: popultaion standard deviation
-- z-score = v / std
zScore :: [Rank] -> Vector Rank
zScore xs = scale (recip std) v
    where 
          x = vector xs
          n = fromIntegral . length $ xs
          meanX = sumElements x / n
          v = x - vector [meanX]
          std = norm_2 v / sqrt n 

-- | A common operation is to subtracte mean 
-- For performance reason, this function may not be exported.
centering :: [Rank] -> Vector Rank
centering xs = v - vector [vmean]
    where 
        v = vector xs
        n = fromIntegral . length $ xs
        vmean = sumElements v / n 

-- | merge two sorted list

--sortH :: Ord a => [a] -> [a] -> [a]
--sortH l1@(x:xs) l2@(y:ys)
--    | x <= y = x : sortH xs l2
--    | otherwise = y : sortH l1 ys
--sortH [] l2 = l2
--sortH l1 [] = l1

pairWiseSimilarity :: Threshold
                      -> SimilarityMatrix       -- fold cum
                      -> RankMetric             -- distance func
                      -> ECount                 -- rating records
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
