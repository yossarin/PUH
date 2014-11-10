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

