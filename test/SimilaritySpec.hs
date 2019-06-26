module SimilaritySpec where

import Test.Hspec
import Test.QuickCheck

import Data.List

import Eris.Compute.Similarity
import ErisTestUtility


spec :: Spec
spec = do
    describe "Sum Absolute Difference" $ do
      it "Naive test: based on L1 norm" $
        sumAbsoluteDifference [1,-1,2,-2] [-1,1,-2,2] `shouldBe` 12
      it "Pro test: symmetric"$ property $
        \l1 l2 -> (length l1 /= length l2)  || (sumAbsoluteDifference l2 l1 == sum (fmap abs (zipWith (-) l1 l2)))
    describe "mean absolute Difference " $do
      it "naive test: based on Sum Absolute Difference" $
        meanAbsoluteDifference tl1 tl2 `shouldBe` sumAbsoluteDifference tl1 tl2 / (fromIntegral . length $ tl1)
      it "Pro test: symmetric" $ property $
        \l1 l2 -> (length l1 /= length l2)  || (meanAbsoluteDifference l2 l1 == sum (fmap abs (zipWith (-) l1 l2)) / fromIntegral (length l2)) 
    describe "Cosine Similarity: " $ do
      it "The vector and itself is one." $ 
        (fromInteger. round) (cosineSimilarity [1.0,1.0,1.0] [1.0,1.0,1.0]) `shouldBe` (1.0:: Double)
      it "The similarity of two linear independent vetors is not 1 " $ 
        diff5Dec (cosineSimilarity [1,2] [1,3]) 1.0 > 0 `shouldBe` True
      it "The similarity of two linear dependent vetors is 1 " $ 
        diff5Dec (cosineSimilarity [1,2] [2,4]) 1.0 `shouldBe` 0
      it " It is 1 for linear dependencies: " $ do
        diff5Dec (cosineSimilarity [-1,2,-3,4,-5] [-4,5,-10,11,-16]) 1.0 `shouldBe` 0

      it "The vector and any perpendicular vector is 0." $ do
        cosineSimilarity [1.0, 0.0] [0.0, 1.0] `shouldBe` 0
        cosineSimilarity [1.0,1.0] [1.0,-1.0] `shouldBe` 0

    describe "msd distance: " $ do
      it "It is 0 with it self. " $ do
        (fromInteger. round) (meanSquaredDistance [1.0,1.0,1.0] [1.0,1.0,1.0]) `shouldBe` (0:: Double)
      it "It is squared L2-norm normal by number of dimension." $ do
        meanSquaredDistance [4.0, 0.0] [0.0, 5.0] `shouldBe` (4^2+5^2)/2
        meanSquaredDistance [1.0,1.0] [1.0,-1.0] `shouldBe` (0+4)/2

    describe "pearson correlation coefficient: " $ do
      it " It is 1 for linear dependencies: " $ do
          (pearsonCC' [-1,2,-3,400,-30] [-4,5,-10,1199,-91]) `shouldBe` (1.0:: Double)
    

    describe "Pearson CC is a special Case of consine similarity: " $ do
        let l1 = [-3..1]
            l2 = fmap (\x -> x*3 120) l1
        it " The value of PCC and CosineSi: " $ 
          (pearsonCC l1 l2) `shouldBe` (cosineSimilarity l1 l2)
        it "pearson CC base on z score: " $ 
          (pearsonCC l1 l2) `shouldBe` (pearsonCC' l1 l2)
    where 
        tl1 = [1,-1,2,-2]
        tl2 = [-1,1,-2,2]
        diff5Dec = relativeEq 5
