module SimilaritySpec where

import Test.Hspec
import Test.QuickCheck
import qualified Numeric.LinearAlgebra as NL

import Data.List

import Eris.Compute.Similarity
import ErisTestUtility


spec :: Spec
spec = do
    describe "L1-norm based. Property tests:" $ do
      it "SAD is L1 norm of the difference of two vectors:" $ property $
        \v1 v2 -> (length v1 /= length v2) || sumAbsoluteDifference v1 v2 == sum ( abs <$> zipWith (-) v1 v2)
      it "SAD is symmetric"$ property $ -- update this property
        \l1 l2 -> (length l1 /= length l2)  || (sumAbsoluteDifference l2 l1 == sum (fmap abs (zipWith (-) l1 l2)))
      it "manhattanDistance is SAD" $ property $
        \v1 v2 -> (length v1 /= length v2) || manhattanDistance v1 v2 == sumAbsoluteDifference v1 v2
      it "taxicab distance is SAD" $ property $
        \v1 v2 -> (length v1 /= length v2) || taxicab v1 v2 == sumAbsoluteDifference v1 v2
      it "mean absolute distance is SAD average on number of dimension" $ property $
        \v1 v2 -> (length v1 /= length v2) || meanAbsoluteDifference v1 v2 == average (abs <$> zipWith (-) v1 v2 :: [Double])
    describe "mean absolute Difference " $do
      it "naive test: based on Sum Absolute Difference" $
        meanAbsoluteDifference tl1 tl2 `shouldBe` sumAbsoluteDifference tl1 tl2 / (fromIntegral . length $ tl1)
      it "Pro test: symmetric" $ property $
        \l1 l2 -> (length l1 /= length l2)  || (meanAbsoluteDifference l2 l1 == sum (fmap abs (zipWith (-) l1 l2)) / fromIntegral (length l2)) 
   
    describe "based on l2-norm" $ do
      it "euclidean distance is l2-norm" $ 
        euclideanDistance tl1 tl2 `shouldBe` NL.norm_2 (NL.vector tl1 - NL.vector tl2)
    
    describe "mse distance: " $ do
      it "It is 0 with it self. " $ 
        (fromInteger. round) (meanSquaredDistance [1.0,1.0,1.0] [1.0,1.0,1.0]) `shouldBe` (0:: Double)
      it "It is squared L2-norm normal by number of dimension." $ do
        meanSquaredDistance [4.0, 0.0] [0.0, 5.0] `shouldBe` (4^2+5^2)/2
        meanSquaredDistance [1.0,1.0] [1.0,-1.0] `shouldBe` (0+4)/2
   
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



    describe "pearson correlation coefficient: " $ do
      it " It is 1 for linear dependencies: " $ do
          (pearsonCC' [-1,2,-3,400,-30] [-4,5,-10,1199,-91]) `shouldBe` (1.0:: Double)
    

    describe "Pearson CC is a special Case of consine similarity: " $ do
        let l1 = [-3..1]
            l2 = fmap (\x -> x*3+120) l1
        it " The value of PCC and CosineSi: " $ 
          (pearsonCC l1 l2) `shouldBe` (cosineSimilarity l1 l2)
        it "pearson CC base on z score: " $ 
          (pearsonCC l1 l2) `shouldBe` (pearsonCC' l1 l2)
    where 
        tl1 = [1,-1,2,-2]
        tl2 = [-1,1,-2,2]
        diff5Dec = relativeEq 5
