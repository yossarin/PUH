import Data.Char
import Data.List

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

headsOf :: [[a]] -> [a]
headsOf []      = []
headsOf ([]:xs) = headsOf xs
headsOf (x:xs)  = head x : headsOf xs

modMult _ _ [] = []
modMult n m (x:xs) = n `mod` m * x : modMult n m xs


addPredecessor :: Num a => [a] -> [a]
addPredecessor []       = []
addPredecessor (x:xs) = addP 0 (x:xs)
  where addP _ [] = []
        addP p (x:xs) = x + p : addP x xs

equalTriplets []           = []
equalTriplets ((x,y,z):xs) = if x==y && y == z then (x,y,z):[] ++ equalTriplets xs else equalTriplets xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

