{-
Third homework.
-}

import Data.Char
import Data.List
import Data.List.Split

pairToList :: (a,a) -> [a]
pairToList (a,b) = [a,b]

interleave :: [a] -> [a] -> [a]
interleave a b =  concat [pairToList x | x <- zip a b]

slice :: Int -> Int -> [a] -> [a]
slice i j l
  | i < 0 || j < 0 || i >= length l || j >= length l = error "Slice index out of range"
  | j > i = take (j - i + 1) $ drop i l
  | otherwise = take (i - j + 1) $ drop j l


decamel :: String -> String
decamel s | (decamel' s)!!0 == ' ' = drop 1 (decamel' s)
          | otherwise = (decamel' s)

decamel' :: String -> String
decamel' "" = error "identifier is empty"
decamel' s | ' ' `elem` s = error "input not in camel case format"
decamel' (x:[]) | isUpper x = " " ++ [toLower x]
                | otherwise = [x]
				
decamel' (x:xs) | xs == "" = [toLower x]
                | isUpper x = " " ++ [toLower x] ++ decamel' xs
                | otherwise = [x] ++ decamel' xs
				
count :: Eq a => [a] -> a -> Int
count l e = length [el | el <- l, el == e]

removeUniques :: Eq a => [a] -> [a]
removeUniques l = [e | e <- l, count l e > 1]

type Mask = String
mask :: String -> Mask -> String
mask _ "" = ""
mask s m = [e | (e,b) <- zip s $ take (length s) (cycle m), b == '1']

type Point = (Int, Int)
type Friend = (Point, String)

distance :: Floating a => Point -> Point -> a
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2

findFriend :: Point -> [Friend] -> String
findFriend _ [] = error "Nobody exists to be your friend"
findFriend p l = snd $ snd $ minimum $ zip [distance p (fst e) | e <- l] l

mulTable :: Int -> [[Int]]
mulTable n 
  | n < 1 = error "Given number lesser than 1"
  | otherwise = [[x*j | x <- i ] | i <- [[1..n]], j <- [1..n]]
  
leftpad :: Show a => Int -> a -> String
leftpad n el 
  | n < 0 = error "Cannot pad to negative length"
  | length (show el) > n = error $ "102 does not fit into " ++ show n ++ " characters"
  | otherwise = (take (n - (length (show el))) (cycle " ")) ++ (show el)

  
prettyTable :: Show a => [[a]] -> IO ()
prettyTable l = do
  let maxLen = maximum [length $ show e | e <- concat l]
  putStrLn $ intercalate "\n" [intercalate " " [ leftpad maxLen e | e <- row] | row <- l]
  
type Separator = String
type Document = String
type CSV = [Entry]
type Entry = [Field]
type Field = String
doc = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"
brokenDoc = "One;Two\nThree;Four;Five"

isValid :: CSV -> Bool
isValid s = (length $ nub $ [length field | field <- s]) == 1

parseCSV :: Separator -> Document -> CSV
parseCSV delim s 
  | delim!!0 `notElem` s = error $ "The character '"++delim++"' does not occur in the text"
  | not $ isValid $ [splitOn delim entry | entry <- (splitOn "\n" s)] = 
  error "The CSV file is not well-formed"
  | otherwise = [splitOn delim entry | entry <- (splitOn "\n" s)]
  
showCSV :: Separator -> CSV -> Document
showCSV delim csv
  | not $ isValid csv = error "The CSV file is not well-formed"
  | otherwise = intercalate "\n" [intercalate delim entry | entry <- csv]
  
colFields :: Int -> CSV -> [Field]
colFields i csv 
  | i >= (length $ csv!!0) || i < 0 = 
  error $ "There is no column "++(show i)++" in the CSV document"
  | otherwise = [entry!!i | entry <- csv]

  
readCSV :: Separator -> FilePath -> IO CSV
readCSV delim filePath = do
  f <- readFile filePath
  return $ parseCSV delim f
  

writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV delim filePath csv = do
  io <- writeFile filePath $ showCSV delim csv
  return io
  

wc :: FilePath -> IO ()
wc fp = do
  f <- readFile fp
  putStrLn $ (show $ length $ splitOn "\n" f)++" "++
             (show $ length $ words f)++" "++
             (show $ length f)

paste :: FilePath -> FilePath -> IO ()
paste fp1 fp2 = do
  f1 <- readFile fp1
  f2 <- readFile fp2
  putStrLn $ intercalate "\n" [l1++"\t"++l2 | (l1,l2) <- zip (splitOn "\n" f1) (splitOn "\n" f2)]
  

cut :: String -> Int -> FilePath -> IO ()
cut delim i fp = do
  f <- readFile fp
  putStrLn $ intercalate "\n" [(splitOn delim ln)!!(i-1) | ln <- lines f]






