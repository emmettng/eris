{-# LANGUAGE OverloadedStrings #-}

module Eris.Typhon.Shoulders
    (
      trivialEcount
    , trivialSimilarityMatrix
    ) where
import qualified Data.HashMap.Strict as Map

import Eris.Meta.DataStructure

-- | For test Similarity Module
trivialEcount :: ECount
trivialEcount = Map.fromList [("user1",esm1),("user2",esm2)]
    where
        esm1 = Map.fromList [("a",2),("b",3),("d",5)]
        esm2 = Map.fromList [("e",5),("a",5),("d",3)]

-- | for test Similairty Module
trivialSimilarityMatrix :: SimilarityMatrix
trivialSimilarityMatrix = Map.empty
