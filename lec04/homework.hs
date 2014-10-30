import Data.List

{-
    1. assignment solved with a simple helper function.
-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n < 0 = 
  error "Cannot do factorial function on a negative number."
factorial n = n * factorial (n - 1)

leftFactorial :: Integer -> Integer
leftFactorial 0 = 0
leftFactorial n | n < 0 = 
  error "Cannot do leftFactorial function on a negative number."
leftFactorial n = sum [factorial i | i <- [0..(n - 1)]]


{-
    2. Function computes the number of zeroes factorial n ends with.
	
	Naive solution:
	length $ takeWhile (=='0') $ reverse $ show $ factorial $ fromIntegral n
	
	The real deal, accredited to http://www.purplemath.com/modules/factzero.htm
-}
divByPow5 :: Int -> Int -> Int
divByPow5 x pow = x `div` 5^pow

factorialZeroes :: Int -> Int
factorialZeroes n = sum $ takeWhile (>0) [divByPow5 n i | i <- [1..]]


{-
    3. Using a helper function to flatten the list of pairs into list
-}
flattenTuple :: [(a,a)] -> [a]
flattenTuple [] = []
flattenTuple ((a,b):rem) = a : b : flattenTuple rem

interleave :: [a] -> [a]
interleave l | even $ length l = flattenTuple $ zip (take half l) (drop half l)
             | odd  $ length l = (flattenTuple $ zip (take (half+1) l) (drop (half+1) l)) ++ [l!!half]
			  where half = length l `div` 2
			         
	
{-
    4. Using a comprehension we get all the subsets of XS size 2. Wooord!
-}
pairs :: [a] -> [(a, a)]
pairs     [] = []
pairs (x:[]) = []
pairs     xs = [(xs!!x, xs!!y) | x <- [0..(len - 2)], y <- [(x + 1)..(len - 1)] ]
              where len = length xs
			  
{-
    5. 
-}
shortestSub :: Eq a => [a] -> [a]
shortestSub [] = []
shortestSub l  = concat $ take 1 [e | e <- drop 1 (inits l), (take len (cycle e)) == l]
                   where len = length l
				   
{-
    6. 
-}
type Timestamp = [Int]

-- (a) isValidTimestamp checks if a timestamp contains valid values.
isValidTimestamp :: Timestamp -> Bool
isValidTimestamp        [] = False
isValidTimestamp       [s] = s >= 0
isValidTimestamp    [m, s] = s >= 0 && m >= 0 && m <= 59
isValidTimestamp [h, m, s] = s >= 0 && m >= 0 && m <= 59 && h >= 0 && h <= 23
isValidTimestamp         _ = False

-- (b) timestampToSec converts a given timestamp to seconds.
toSec :: Timestamp -> Int
toSec       [s] = s
toSec    [m, s] = m*60 + s
toSec [h, m, s] = h*60*60 + m*60 + s

timestampToSec :: Timestamp -> Int
timestampToSec tstmp | isValidTimestamp tstmp = toSec tstmp
                     | otherwise              = error "Invalid timestamp"
					 
-- (c) timeDiff calculates a temporal difference, in seconds, between two timestamps.
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff a b = abs $ timestampToSec a - timestampToSec b


{-
    7. 
-}

-- (a) counts given a list of elements,	
--     returns a list of pairs of (element, number of occurrences of the element)
counts :: Ord a => [a] -> [(a, Int)]
counts l = [(e!!0, length e) | e <- group $ sort l]

-- (b) This hint sucks -> Hint: count the number of an element’s occurences in the list.
group' :: Eq a => [a] -> [[a]]
group' l = [[e | e <- l, e == el] | el <- nub l ]

-- (c) 
counts' :: Eq a => [a] -> [(a, Int)]
counts' xs = [(e!!0, length e) | e <- group' xs]


{-
    8. Lights Out!
-}
type Grid = [String]

-- (a) lightsOutLite takes a Grid and computes the minimal number of moves 
--     to complete a simplified version of Lights Out!.
isWellFormed :: Grid -> Bool
isWellFormed l | l == [[]] = False
               | otherwise = length (nub [length e | e <- l ])== 1
			   
lightsOutLite :: Grid -> Int
lightsOutLite xss | isWellFormed xss = length [e | e <- concat xss, e == '1']
                  | otherwise        = error "Broken grid!"
				  
-- (b) TODO: lightsOut computes the minimal number of moves necessary 
--     to complete the original version of the game.


{-
    8. One Edits, Two Edits !
-}
-- testing the oneEdits and twoEdits functions
compareToFile :: (String -> [String]) -> String -> FilePath -> IO Bool
compareToFile f s file = do
  list <- readFile file
  let l = read list :: [String]
  putStrLn $ show $ (f s) \\ l
  putStrLn $"Function ln:" ++ show (length (f s )) ++ " || file ln: " ++ show (length l)
  return $ {-length (f s ) == length l -- &&-} and [e `elem` l | e <- (f s)]
  
testOneEdits :: IO Bool
testOneEdits = compareToFile oneEdits "hi" "oneEdits.txt"

testTwoEdits :: IO Bool
testTwoEdits = compareToFile twoEdits "hi" "twoEdits.txt"

-- (a) Function oneEdits, given a String, returns all possible
--     strings that are one edit–distance away from it.
oneEdits :: String -> [String]
oneEdits s = nub $ [l!!i ++ [e] ++ r!!i | i <- [0..length s], e <- ['a'..'z']] ++ 
             [l!!i ++ r!!(i+1) | i <- [0..(length s - 1)]] ++         
			 [l!!i ++ [e] ++ r!!(i+1) | i <- [0..(length s - 1)], e <- ['a'..'z'], e /= s!!i] ++
			 [l!!i ++ [s!!(i+1),s!!i] ++ r!!(i+2) | i <- [0..(length s - 2)]]
               where l = inits s
                     r = tails s 
{-f1 s = [l!!i ++ [e] ++ r!!i | i <- [0..length s], e <- ['a'..'z']]
f2 s = [l!!i ++ [e] ++ r!!(i+1) | i <- [0..(length s - 1)], e <- ['a'..'z'], e /= s!!i]
	where l = inits s
          r = tails s -}

twoEdits :: String -> [String]
twoEdits _ = []






















