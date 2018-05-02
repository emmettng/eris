{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Eris.Typhon.Shoulders
    (
      trivialEcount
    , trivialSimilarityMatrix
    , psTestData
    ) where
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Data.Vector
import Control.Monad.Except

import Eris.Meta.DataStructure

-- | For test Similarity Module
-- instance of Ecount
trivialEcount :: ECount
trivialEcount = Map.fromList [("user1",esm1),("user2",esm2)]
    where
        esm1 = Map.fromList [("a",2),("b",3),("d",5)]
        esm2 = Map.fromList [("e",5),("a",5),("d",3)]

-- | for test Similairty Module
-- An empty Map for pairWiseSimilarity
trivialSimilarityMatrix :: SimilarityMatrix
trivialSimilarityMatrix = Map.empty


-- | Get testing data from pairWiseSimilarity from a .csv file
psCSVData :: String
psCSVData = "./source/test_dataset.csv"

-- psTestData :: MonadIO m => m

psTestData :: ExceptT String IO (Vector SaleRecord)
psTestData = do
    csvData <- liftIO $ BL.readFile psCSVData
    case decode NoHeader csvData of
        Left err -> fail err
        Right v -> return v

records2eCount :: Vector SaleRecord -> GroupName -> ECount
records2eCount = undefined
