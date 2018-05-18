module Main where
import Test.QuickCheck
import Data.List

prop_revapp :: [Char] -> [Char] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

-- deepCheck = quickCheckWith (stdArgs {maxSuccess = 10000})


prop_split_inv xs = forAll (elements xs) $ testXS xs
    where testXS xxs c =
                        let y = split c xxs
                        in collect((length y, length xxs)) $ unsplit c y == xxs

--    \c -> collect ((length (split c xs), length xs)) $ unsplit c (split c xs) == xs

split :: Char  -> String -> [String]
split c str = xs : if null xss then [] else split c (tail xss)
    where xs = takeWhile (/=c) str
          xss = dropWhile (/=c) str

unsplit :: Char -> [String]-> String
unsplit c = concat . intersperse [c]

examples = [('@',"pbv@dcc.fc.up.pt"), ('/',"/usr/include")]

test (c,xs) = unwords ["split", show c, show xs, "=", show ys]
    where ys = split c xs

--main = mapM_ (putStrLn . test) examples
main = quickCheck prop_split_inv
