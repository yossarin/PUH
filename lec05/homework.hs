import Data.Char


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

rpnCalc :: String -> Int
rpnCalc =  head . foldl foldingFunction [] 
    where   foldingFunction (x:y:ys) '*' = (x * y):ys
            foldingFunction (x:y:ys) '+' = (x + y):ys
            foldingFunction (x:y:ys) '-' = (y - x):ys
            foldingFunction (x:y:ys) '^' = (y ^ x):ys
            foldingFunction (x:y:ys) '/' = (y `div` x):ys
            foldingFunction xs numberString = digitToInt numberString:xs






