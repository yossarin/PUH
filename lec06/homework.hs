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
scan f z (x:xs) =  z: scan f (f z x) xs