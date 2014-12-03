import Data.Ord
import Data.List

{-
  1.) My versions of existing Data.List higher order functions :)
-}

-- a) takeWhile' takes list elements as long as the predicate evaluates to True.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []
                    
-- b) dropWhile' drops list elements as long as the predicate evaluates to True.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                   = []
dropWhile' p l@(x:xs) | p x       = dropWhile' p xs
                      | otherwise = l
                      
-- c) zipWith' combines two lists using a given function.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = [f x y] ++ zipWith' f xs ys


{-
  2.) function efficientSortBy does the same as sortBy 
      but calculates each term only once
-}

efficientSortBy :: Ord b => (a -> b) -> [a] -> [a]
efficientSortBy f xs = snd $ unzip $ sortBy (comparing fst) pairs
  where pairs = zip (map f xs) xs
  
{-
  3.) Stemming is the process of reducing inflected words to their stem, 
      or root form, for easier word identification. It is typically done by discarding the word suffix.
-}

-- a) stemming function stemmer1 that discards the suffix up to the last
--    vowel (inclusive), but only if the word with the discarded suffix is
--    at least as long as the suffix itself.
stemmer1 :: String -> String
stemmer1 xs | length stem < prefixl = xs
            | otherwise             = stem
  where vowels = "aeiou"
        stem   = reverse . drop 1 . dropWhile' (`notElem` vowels) . reverse $ xs
        prefixl = length xs - length stem

stemmer2 :: [String] -> String -> String
stemmer2 suffixes word | length stem <= prefixl = word
                       | otherwise             = stem
  where endswith xs suf = and $ zipWith (==) (reverse xs) (reverse suf)
        max_suff        = maximum $ filter (endswith word) suffixes
        stem            = reverse . drop (length max_suff) $ reverse word
        prefixl         = length word - length stem

