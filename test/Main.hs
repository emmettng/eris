module Main  where

import Test.Hspec
import Eris.Compute.Similarity

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Test cosine similarity of two vectors" $
      it "some" $ do
      (consine [1.0,1.0,1.0] [1.0,1.0,1.0]) `shouldBe` 0
