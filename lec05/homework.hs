import Data.Char
import Data.List


{-
  1.) My own version of Data.List.Intercalate.
-}

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [x]     = x
intercalate' e (x:xss) = x ++ e ++ (intercalate' e xss)


{-
  2.) Following functions work with discrete random variables.
-}
type Probability = Double
type DiscreteRandVar = [(Int, Probability)]
x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- (a) explicitly recursive function mean and an accumulator-style recursive
--     function mean' that calculate the mean of a discrete random variable

mean :: DiscreteRandVar -> Double
mean [(x, p)]    = fromIntegral x * p
mean ((x, p):xs) = fromIntegral x * p + mean xs

meanHelp' :: DiscreteRandVar -> Double -> Double
meanHelp' [] n          = n
meanHelp' ((x, p):xs) n = meanHelp' xs (fromIntegral x * p + n)

mean' :: DiscreteRandVar -> Double
mean' x = meanHelp' x 0

-- (b) explicitly recursive function variance and an accumulator-style recursive 
--     function variance' that calculate the variance of a discrete random variable

varianceRec :: DiscreteRandVar -> Double -> Double
varianceRec [(x, p)] m    = (fromIntegral x - m)^2 * p
varianceRec ((x, p):xs) m = (fromIntegral x - m)^2 * p + varianceRec xs m

variance :: DiscreteRandVar -> Double
variance xs = varianceRec xs $ mean' xs

varianceAcc :: DiscreteRandVar -> Double -> Double -> Double
varianceAcc [] n m          = n
varianceAcc ((x, p):xs) n m = varianceAcc xs ((fromIntegral x - m)^2 * p + n) m

variance' :: DiscreteRandVar -> Double
variance' xs = varianceAcc xs 0 $ mean' xs

-- (c) explicitly recursive function probabilityFilter and an accumulator style 
--     recursive function probabilityFilter' that take a probability and a 
--     random variable and return a list of values that have at least the given 
--     probability of appearing, in the same order in which they appear in the 
--     random variable definition

probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter prob ((x, p):[]) | p >= prob = [x]
                                   | otherwise = []
								
probabilityFilter prob ((x, p):xs) | p >= prob = x : probabilityFilter prob xs
                                   | otherwise = probabilityFilter prob xs

probAcc :: Probability -> DiscreteRandVar -> [Int] -> [Int]
probAcc _ [] acc = acc
probAcc prob ((x, p):xs) acc | p >= prob = probAcc prob xs (acc ++ [x])
                             | otherwise = probAcc prob xs acc
								   
probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' prob xs = probAcc prob xs []

{-
  3.) Often we need to split up a list into smaller chunks.
-}

-- (a) function chunk splits up a list xs into sublist of length n. If the
--     length of xs is not a multiple of n, the last sublist will be shorter than n. 

chunk :: Int -> [a] -> [[a]]
chunk 0 _ = []
chunk _ [] = []
chunk n xs = take n xs : chunk n ( drop n xs )

-- (b) function chunkBy splits up a list xs into sublists of lengths given
--     in a list of indices is. If the lengths in is do not add up to the length
--     of xs, the remaining part of xs will remain unchunked

chunkBy :: [Int] -> [a] -> [[a]]
chunkBy [] _ = []
chunkBy _ [] = []
chunkBy (y:ys) xs = take y xs : chunkBy ys (drop y xs)

-- (c) function chunkInto splits up a list xs into n sublists of equal
--     length. If the length of xs is not divisible by n, chunk the 
--     remainder into the last sublist

chunkInto :: Int -> [a] -> [[a]]
chunkInto 0 _  = []
chunkInto _ [] = []
chunkInto n xs
  | (length xs) < 2*t = [xs] 
  | otherwise         = take t xs : chunkInto (n-1) (drop t xs)
    where t = (truncate $ fromIntegral (length xs) / fromIntegral n)

{-
  4.) function rpnCalc that takes a mathematical expression written in
      Reverse Polish notation and calculates its result. 
	  Inspiration: http://learnyouahaskell.com/functionally-solving-problems
-}

op :: [Int] -> Char -> [Int]
op xs@(x:y:ys) c = case c of 
  '*' -> (x * y):ys
  '+' -> (x + y):ys
  '-' -> (y - x):ys
  '^' -> (y ^ x):ys
  '/' -> (y `div` x):ys
  _   -> if isDigit c then digitToInt c:xs else error "Invalid RPN expression"
op xs digit = if isDigit digit then digitToInt digit:xs else error "Invalid RPN expression"

rpnCalc :: String -> Int
rpnCalc str | str == "" = 0
            | otherwise =  head $ foldl op [] str

{-
  5.) 
-}

-- (a) function gcd' calculates the greatest common divisor of two
--     integers, using explicit recursion and the Euclidean algorithm.

gcd' :: Int -> Int -> Int
gcd' a b | b == 0 = a
         | r == 0 = abs b
         | otherwise = gcd' b r
  where q = div a b 
        r = mod a b
		
-- (b) function gcdAll calculates the greatest common divisor of an
--     arbitrary number of integers given in a list.
gcdAll :: [Int] -> Int
gcdAll [] = error "Cannot compute gcd of an empty list"
gcdAll xs = minimum $ [gcd' (l!!j) (l!!i) | i <- [0..len], j <- [i..len]]
  where l   = sort xs
        len = length xs - 1
		
-- (c) function extendedGcd uses the extended Euclidean algorithm
--     to calculate the Bezout coefficients along with the gcd.

gcdEx :: Int -> Int -> Int -> Int -> Int -> Int -> (Int,Int,Int)
gcdEx a b s0 t0 s1 t1 | b == 0 = (s1, t1, a)
                      | r == 0 = (s1, t1, abs b)
                      | otherwise = gcdEx b r s1 t1 (s0 - s1*q) (t0 - t1*q)
               where q = div a b 
                     r = mod a b
		
extendedGcd :: Int -> Int -> (Int,Int,Int)
extendedGcd a b = gcdEx a b 1 0 0 1


{-
  6.) a function isBipartite that takes an unweighted graph 
      represented as an adjacency list and checks whether the 
	  given graph is a bipartite graph. 
-}
type AdjacencyList = [Int]
type Graph = [AdjacencyList]

isBipartite :: Graph -> Bool
isBipartite g | intersect a b == [] = True
              | otherwise          = False
			   where (a, b) = color g [] []
        
color [] a b     = (a, b)
color (x:xs) a b = color xs (head x : a) (tail x ++ b)






