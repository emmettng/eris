module Main  where

import Test.Hspec
import Eris.Compute.Similarity

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Cosine Similarity: " $ do
      it "The vector and itself is one." $ do
        (cosineSimilarity [1.0,1.0,1.0] [1.0,1.0,1.0]) `shouldBe` (1.0:: Double)

      it "The vector and any perpendicular vector is 0." $ do
        cosineSimilarity [1.0, 0.0] [0.0, 1.0] `shouldBe` 0
        cosineSimilarity [1.0,1.0] [1.0,-1.0] `shouldBe` 0
