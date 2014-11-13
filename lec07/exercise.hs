import Data.Char
import Data.List
import Control.Monad

-- EXERCISE 1

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..] 

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- EXERCISE 2

maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (abs . uncurry (-)) . zip xs $ tail xs 

diffUni xs f = f . map (abs . uncurry (-)) . zip xs $ tail xs 

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (diffUni xs minimum, diffUni xs maximum)

-- EXERCISE 3

isTitleCased :: String -> Bool
isTitleCased = all (isUpper . head) .  words

-- EXERCISE 4
elem' y = foldr (\x acc -> acc || x==y) False

reverse' = foldr (\x acc -> acc ++ [x]) [] 

-- EXERCISE 5
reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) [] 

-- sumEven' :: [Integer] -> Integer

