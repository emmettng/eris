{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Eris.Typhon.Shoulders
    (
      trivialEcount
    , empMatrix
    , psTestData
    , updateMap
    ) where
import qualified Data.HashMap.Strict as Map
import Data.Hashable
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

trivialSaleRecords :: Vector SaleRecord
trivialSaleRecords = V.fromList [
                                SaleRecord "BL123T" "C001" 2,
                                SaleRecord "BL123T" "C002" 3,
                                SaleRecord "BL123T" "C003" 5,
                                SaleRecord "BL223T" "C009" 5,
                                SaleRecord "BL223T" "C001" 5,
                                SaleRecord "BL223T" "C003" 3
                              ]
-- | for test Similairty Module
-- An empty Map for pairWiseSimilarity
empMatrix :: SimilarityMatrix
empMatrix = Map.empty

empECount :: ECount
empECount = Map.empty

empESMap :: ESMap
empESMap = Map.empty

-- | Get testing data from pairWiseSimilarity from a .csv file
psCSVData :: String
--psCSVData = "./source/test_dataset.csv"
psCSVData = "./source/sku_phone_50k.csv"

-- psTestData :: MonadIO m => m

psTestData :: ExceptT String IO (Vector SaleRecord)
psTestData = do
    csvData <- liftIO $ BL.readFile psCSVData
    case decode NoHeader csvData of
        Left err -> fail err
        Right v -> return v

records2eCount :: GroupName -> Vector SaleRecord -> ECount
records2eCount gname vec = case gname of
    "sku" -> V.foldr (gEcount sku cid) empECount vec
    "cid" -> V.foldr (gEcount cid sku) empECount vec
    _ -> empECount
    where gEcount :: (SaleRecord -> BL.ByteString) ->     (SaleRecord -> BL.ByteString) -> SaleRecord -> ECount -> ECount
          gEcount fn1 fn2 sr cumMap =
              let key = fn1 sr
                  innerKey = fn2 sr
                  rank = cRanking sr
                  tmpMap = Map.lookupDefault empESMap key cumMap
                  newtmpMap = updateESMap innerKey rank tmpMap
              in Map.insert key newtmpMap cumMap

updateESMap :: (Eq k,Hashable k, Num a) => k -> a -> Map.HashMap k a -> Map.HashMap k a
updateESMap = Map.insertWith (+)


updateAnother :: BL.ByteString -> Double -> ESMap -> ESMap
updateAnother = Map.insertWith (+)

updateMap :: (Eq k, Hashable k, Num v) => k -> v -> Map.HashMap k v -> Map.HashMap k v
updateMap k v m =
    if Map.member k m
      then Map.adjust (+v) k m
    else
      Map.insert k v m
