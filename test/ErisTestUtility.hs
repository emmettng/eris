module ErisTestUtility where 
import Data.List 

average xs = realToFrac (sum xs) / genericLength xs

relativeEq :: Int -> Double -> Double -> Int
relativeEq deci n1 n2 =
    let absd = abs $ n1 - n2
    in round $ absd * 10^deci