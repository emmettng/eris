module Main where

import Test.Hspec
import qualified Test.TestSimilarity as TS

main :: IO ()
main = hspec TS.similarityHspec
