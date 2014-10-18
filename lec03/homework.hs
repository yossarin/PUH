{-
Third homework.
-}

pairToList :: (a,a) -> [a]
pairToList (a,b) = [a,b]

interleave :: [a] -> [a] -> [a]
interleave a b =  concat [pairToList x | x <- zip a b]

slice :: Int -> Int -> [a] -> [a]
slice i j l
  | i < 0 || j < 0 || i >= length l || j >= length l = error "Slice index out of range"
  | j > i = take (j - i + 1) $ drop i l
  | otherwise = take (i - j + 1) $ drop j l