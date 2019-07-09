module Main where 

import Test.QuickCheck
import Data.List (intersperse)
import Data.Numbers.Primes

split :: Char -> String -> [String]
split c [] = [""]
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c)  xs
          xs'' = dropWhile (/=c)  xs

unsplit :: Char -> [String] -> String 
unsplit c = concat . intersperse [c]

prop_split_inv  xs
    = collect (length xs) $ forAll (elements xs) $ \c ->
      unsplit c (split c xs) == xs

-- | Google: a quickcheck-tutorial-generators
--
--
--
data Tree a = 
    Tree a [Tree a] deriving (Show)

nodes :: Tree a -> Int 
nodes (Tree _ []) = 2 
nodes (Tree _ xs) = 2 + auxiCountList xs 
  where auxiCountList ts = sum $ nodes <$> ts

edges :: Tree a -> Int 
edges (Tree _ []) = 1
edges (Tree _ xs) = 1 + subTreeNum + subTreeEdges
  where subTreeEdges = sum $ edges <$> xs 
        subTreeNum = length xs


instance Arbitrary a => Arbitrary (Tree a) where 
  arbitrary =
    sized arbitrarySizedTree 
  
arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  t <- arbitrary
  n <- choose(0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div`4))
  return (Tree t ts)
 
prop_OneMoreNodeThanEdges :: Tree Int -> Property 
prop_OneMoreNodeThanEdges tree = 
  collect ((nodes tree, edges tree)) $ nodes tree == edges tree + 1

-- | fpcomplete.com/blog/2017/01/quickcheck
--  
--
newtype Prime a = Prime a deriving Show 

instance (Integral a, Arbitrary a) => Arbitrary (Prime a) where 
  arbitrary = do 
    x <- frequency [ (10, choose(0,1000))
                    , (5, choose(1001,10000))
                    , (1, choose(10001,50000))
                    ]
    return $ Prime (primes !! x)

prop_PrimeSum_v4 :: Prime Int -> Prime Int -> Property 
prop_PrimeSum_v4 (Prime p) (Prime q) = 
  p > 2 && q > 2 ==> classify (q < 1000 || q < 1000) "has small prime " $ 
  collect( if p<q then (p,q) else (q,p)) $ even (p + q)


main :: IO ()
main = verboseCheck prop_PrimeSum_v4
-- main = quickCheckWith stdArgs{maxSuccess = 500} prop_PrimeSum_v4
-- main = quickCheck prop_PrimeSum_v4 
-- main = quickCheck prop_OneMoreNodeThanEdges
-- main = quickCheck prop_split_inv 

-- | Implication operator (function)
-- 1. collect
-- 2. forall
-- 3. ==> 
-- 4. classify
-- 5. frequency 

-- | others:
-- 1. verboseCheck

-- : NEED TO KNOW MORE ABOUT Testable
-- info quickCheck
-- quickCheck :: Testable prop => prop -> IO ()
-- 
-- *Main Lib Test.QuickCheck> :info collect 
-- collect :: (Show a, Testable prop) => a -> prop -> Property
--   	-- Defined in ‘Test.QuickCheck.Property’
-- *Main Lib Test.QuickCheck> :info forAll
-- forAll ::
--   (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
--   	-- Defined in ‘Test.QuickCheck.Property’
-- 
--     *Main Lib Test.QuickCheck> :info Testable 
--     class Testable prop where
--       property :: prop -> Property
--       {-# MINIMAL property #-}
--         -- Defined in ‘Test.QuickCheck.Property’
--     instance [safe] Testable Property
--       -- Defined in ‘Test.QuickCheck.Property’
--     instance [safe] Testable prop => Testable (Gen prop)
--       -- Defined in ‘Test.QuickCheck.Property’
--     instance [safe] Testable Discard
--       -- Defined in ‘Test.QuickCheck.Property’
--     instance [safe] Testable Bool
--       -- Defined in ‘Test.QuickCheck.Property’
--     instance [safe] (Arbitrary a, Show a, Testable prop) =>
--                     Testable (a -> prop)    === make multi variable function possible
--       -- Defined in ‘Test.QuickCheck.Property’
--     instance [safe] Testable ()
--       -- Defined in ‘Test.QuickCheck.Property’
    