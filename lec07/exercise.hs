import Data.Char
import Data.List
import Control.Monad

-- EXERCISE 1

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..] 

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concat . map (\x -> (toUpper . head $ x):".") . filter (p) . words 

-- EXERCISE 2

maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (abs . uncurry (-)) . zip xs $ tail xs 

diffUni xs f = f . map (abs . uncurry (-)) . zip xs $ tail xs 

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (diffUni xs minimum, diffUni xs maximum)


-- EXERCISE 3

isTitleCased :: String -> Bool
isTitleCased = all (isUpper . head) .  words

sortPairs :: Ord b => [(a,b)] -> [(a,b)]
sortPairs = sortBy (\(_,x) (_,y)-> compare x y )

filename :: String -> String
filename = reverse . takeWhile (/='/') . reverse

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices xs = findIndices (==maximum xs) xs

-- EXERCISE 4
elem' y = foldr (\x acc -> acc || x==y) False

reverse' = foldr (\x acc -> acc ++ [x]) []

nubRuns :: Eq a => [a] -> [a]
nubRuns (x:xs) = foldr (\x acc -> acc ++ (if (last acc) == x then [] else [x])) [x] xs

-- EXERCISE 5
reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) [] 

-- sumEven' :: [Integer] -> Integer

