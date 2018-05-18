module Eris.Predict.KNNbased where

import Data.HashMap.Strict as Map
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad (guard)
import qualified Numeric.LinearAlgebra as NL

import Eris.Meta.DataStructure
import Eris.Compute.Similarity

-- | Example:
-- records2eCount :  get ecount
-- pairWiseSimilarity : get similarity Matrix
-- neighborOf : get neighbor of a entity  A
-- f1 : filter neighbors of A which related to entity of the other type .
-- c1 : retrive relation between two type of entity.
-- t : compute knnbased recommendataion.
t :: NeighborSimilarity-> NeighborRank -> Maybe Rank
t ns nr
    | length ns /= length nr = Nothing
    | otherwise = Just $ knnbased  ns nr
    where knnbased ns nr =  let
                              v1 = NL.vector $ fmap snd ns
                              v2 = NL.vector $ fmap snd nr
                            in  v1 NL.<.> v2 / NL.norm_1 v1


f1 :: ECount -> NeighborSimilarity -> PID -> NeighborSimilarity
f1 ecount nsim pid = do
    r@(eid,_) <- nsim
    guard $ validRecord ecount eid pid
    return r

c1 :: ECount -> NeighborSimilarity -> PID -> NeighborRank
c1 ecount nsim pid =do
                    (eid, _) <- nsim
                    let justRank = getRank ecount eid pid
                    guard $ isJust justRank
                    return (eid, fromJust justRank)

-- | Given an ID of a entity of certain type.
--  Based on the similairty matrix of this type of entity.
--  collect its neighbors and sort by Order.
--  return a list of tuple [(EID, Rank)] , rank value is necessary in case any further filter or verification process.
neighborOf :: SimilarityMatrix -> Order -> CID -> NeighborSimilarity
neighborOf matrix ord cus
  | isNothing (Map.lookup cus matrix) = []
  | otherwise = let
      rlist = Map.toList matrix
      neighborsRank = collectRank rlist cus
      in sortByRank neighborsRank ord
  where
        collectRank :: [(EID, Map.HashMap EID Rank)] -> EID -> NeighborSimilarity
        collectRank ((eid,edict) : exs) tid
            | eid == tid = Map.toList edict
            | otherwise = (tid,Map.lookupDefault 0 tid edict): collectRank exs tid
        sortByRank :: NeighborSimilarity -> Order -> NeighborSimilarity
        sortByRank nr o = let
            ascOrder = quickSortRank nr
            in if o == Desc
               then
                 reverse ascOrder
                else
                  ascOrder
        quickSortRank :: NeighborSimilarity -> NeighborSimilarity
        quickSortRank [] = []
        quickSortRank ((eid,erank):ers) =
            let smallRanks = quickSortRank [e | e <- ers , snd e <= erank]
                biggerRanks = quickSortRank [ e | e<- ers, snd e > erank]
            in smallRanks ++ [(eid,erank)] ++ biggerRanks

-- | Auxiliary Functions
validRecord :: ECount -> CID -> PID -> Bool
validRecord ec c p=
        let cContainsP = do
                          pdict <- Map.lookup c ec
                          Map.lookup p pdict
        in isNothing cContainsP

getRank :: ECount -> CID -> PID -> Maybe Rank
getRank ec c p = do
    pdict <- Map.lookup c ec
    Map.lookup p pdict
