import Data.Char


{-
 1. a function partition partitions a list using a user-provided 
    predicate function
-}
partition :: [a -> Bool] -> [a] -> [[a]]
partition fs xs = [ filter f xs | f <- fs]


{-
 2. function cycleMap fs xs maps various functions from fs over
    a list xs, depending on the index of an element in the list.
	The list fs of functions to be mapped is cycled over the list
	xs: the first function from fs is applied on the first element
	from xs, the second function from fs on the second element from 
	xs, etc. When the list of functions fs is exhausted, mapping 
	restarts from the first function from fs
-}
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] xs = []
cycleMap fs xs = [ f x | (f, x) <- zip (cycle fs) xs] 

{-
 3. 
-}
-- (a) function reduce reduces a list of elements
--     to a single element using a seed value and a binary reduction function.
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ seed []     = seed
reduce f seed (x:xs) = f (reduce f seed xs) x

-- (b) reduce1 that behaves like reduce, but assumes the input list contains 
--     at least one element and so eschews taking a seed element. 
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ []     = error "reduce1 got an empty list"
reduce1 f (x:[]) = x
reduce1 f (x:xs) = f x (reduce1 f xs)

-- (c) function scan performs similarly to reduce, but returns a list of
--     all the intermediate values with the result at the end instead of 
--     just the last result.
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ z []     = [z]
scan f z (x:xs) =  z : scan f (f z x) xs

{-
  4.)
-}
-- (a) rreduce performs similarly as reduce, only it does the operations
--     from right to left, instead.
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce _ seed []     = seed
rreduce f seed (x:xs) = f x (rreduce f seed xs)

-- (b) rreduce1 behaves like rreduce, but assumes 
--     the input list contains at least one element.
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ []     = error "rreduce1 got an empty list"
rreduce1 f (x:[]) = x
rreduce1 f (x:xs) = f x (rreduce1 f xs)

-- (c) variant of the scan function that works from right to left, called rscan.
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan _ z []     = [z]
rscan f z (x:xs) =  f x (head rsc) : rsc
  where rsc = rscan f z xs
  
  
{-
  5.)
-}
-- (a) function newton computes an approximation of the square root of
--     a number using a special case of Newton’s method.
type Tolerance = Double
newton :: Tolerance -> Double -> Double
newton t x = snd $ head $ dropWhile (\(n1, n2) -> n1 - n2 > t) $ zip iter $ tail iter 
  where
    iter = iterate y' (x+1)
    y' y = (y+x/y)/2

-- (b) function deriv computes the derivative of a given function.
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x+dx) - f x)/dx 
  where dx = 0.00001
  
  
{-
  6.)  function isHappy checks whether a number is a happy number. 
       happy number = http://en.wikipedia.org/wiki/Happy_number
-}
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

isHappy :: Int -> Bool
isHappy = f [] where
    f _  1 = True
    f xs n = notElem n xs && f (n:xs) (sum . map (^ 2) $ digs n)



