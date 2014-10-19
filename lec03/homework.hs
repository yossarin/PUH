{-
Third homework.
-}

import Data.Char

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

-- distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
-- findFriend :: Point -> [Friend] -> String
findFriend _ [] = error "Nobody exists to be your friend"
findFriend p l = snd $ snd $ minimum $ zip [distance p (fst e) | e <- l] l
