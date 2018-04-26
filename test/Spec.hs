module Main  where

import Test.Hspec
import Eris.Compute.Similarity

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Test cosine similarity of two vectors" $ do
      it "The vector and itself" $ do
        (consine [1.0,1.0,1.0] [1.0,1.0,1.0]) `shouldBe` 1.0

      it "The vector and any perpendicular" $ do
        consine [1.0, 0.0] [0.0, 1.0] `shouldBe` 0
        consine [1.0,1.0] [1.0,-1.0] `shouldBe` 0
