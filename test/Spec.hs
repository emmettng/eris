module Main  where

import Test.Hspec
import Eris.Compute.Similarity

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Cosine Similarity: " $ do
      it "The vector and itself is one." $ do
        (fromInteger. round) (cosineSimilarity [1.0,1.0,1.0] [1.0,1.0,1.0]) `shouldBe` (1.0:: Double)
      it "The similarity of two linear dependent vetors is 1 " $ do
        (fromIntegral . round) (cosineSimilarity [1,2] [1,3]) `shouldBe` (1.0 :: Double)

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
          (pearsonCC [-1,2,-3,4,-5] [-4,5,-10,11,-16]) `shouldBe` (1.0:: Double)

    describe "Pearson CC is a special Case of consine similarity: " $ do
        let l1 = [-3..1]
            l2 = fmap (\x -> x*300 -120) l1
        it " The value of PCC and CosineSi: " $ do
          (pearsonCC l1 l2) `shouldBe` (cosineSimilarity l1 l2)
        it "pearson CC base on z score: " $ do
          (pearsonCC l1 l2) `shouldBe` (pearsonCC' l1 l2)
