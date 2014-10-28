

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
