import Data.Char
import Data.List

addPairs :: [(Int,Int)] -> [Int]
addPairs xs = [x+y | (x,y) <- xs]

removeEverySecond :: String -> String
removeEverySecond s = unwords [w|(i,w)<-zip [1..] (words s), even i]

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

concatThree :: String -> String -> String -> String
concatThree s1 s2 s3 = s1 ++ s2 ++ s3

trimBy :: Int -> String -> String
trimBy n xs = reverse $ drop n $ reverse $ drop n xs
