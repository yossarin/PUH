import Data.List
import Data.Char

l1 = [1,2,3]

l1' = (1:(2:(3:[])))

l1'' = 1:2:3:[]

l2 = [1,2,3]++[4,5,6]

myConcat l1 l2 = l1 ++ l2

listify x = x:[]

listify' x = [x]

l3 = take 3 [9,2,10,3,4]

l4 = drop 3 [9,2,10,3,4]

l5 = reverse [1,2,4]

isPalindrome s = s == reverse s

l9 = repeat 'a'

l10 = cycle [1,2,3]

l11 = replicate 10 'a'

replicate' n x = take n $ repeat x

trim l = tail (init l)
trim' l = init $ tail l

l21 = [[1,2,3],[4,5,6],[7,8,9,10]]

l22 = ["red", "green", "blue"]

l23 = concat l21

dropTake3 l = drop 3 $ take (length l - 3) l

initials s1 s2 = head s1 : ". " ++ [head s2] ++ "."

conLong s1 s2 | length s1 > length s2 = s1 ++ s2
              | otherwise = s2 ++ s1
			  
safeHead l | l == [] = []
           | otherwise = [head l]

hasDuplicates l | length (nub l) == length l = False
                | otherwise = True
				
doublesFromTo a b 
  | a < b = [x*2 | x <- [a..b]]
  | otherwise = [x*2 | x <- [b..a]]