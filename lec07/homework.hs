import Data.Ord
import Data.List
import Data.Char

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
      or root form, for easier word identification. It is typically done
      by discarding the word suffix.
-}

-- a) stemming function stemmer1 discards the suffix up to the last
--    vowel (inclusive), but only if the word with the discarded suffix is
--    at least as long as the suffix itself.
stemmer1 :: String -> String
stemmer1 xs | length stem < prefixl = xs
            | otherwise             = stem
  where vowels = "aeiou"
        stem   = reverse . drop 1 . dropWhile' (`notElem` vowels) . reverse $ xs
        prefixl = length xs - length stem

-- b) a stemming function stemmer2 discards a suffix if it is in a provided list of
--    suffixes. If the word contains none of the listed suffixes, it remains unchanged.
--    If it is possible to discard more than one suffix, discard the longest suffix.
--    A suffix may only be discarded if the remainder is at least as long as the suffix.
stemmer2 :: [String] -> String -> String
stemmer2 suffixes word | length stem < prefixl = word
                       | otherwise              = stem
  where endswith xs suf = and $ zipWith (==) (reverse xs) (reverse suf)
        ends            = filter (endswith word) suffixes
        max_suff        = if ends == [] then "" else  maximumBy (comparing length) $ ends
        stem            = reverse . drop (length max_suff) $ reverse word
        prefixl         = length word - length stem

-- c) stemming function stemmer3 works like stemmer2, but uses
--    stemmer1 instead if none of the suffixes appear in the word.
stemmer3 :: [String] -> String -> String
stemmer3 suffixes word | word == stem2 = stemmer1 word
                       | otherwise     = stem2
                       where stem2 = stemmer2 suffixes word

-- d) function testStemmer tests the precision of a stemmer. It takes
--    a sample as a list of pairs (word, correctSuffix) and a stemming function,
--    then calculates the percentage of words that were correctly stemmed by the
--    stemmer.
testStemmer :: [(String, String)] -> (String -> String) -> Double
testStemmer pairs f = fromIntegral (length correct) / (fromIntegral $ length pairs) * 100
  where results = map (f . fst) pairs
        correct = filter (\(x,y) -> x == y) . zip results $ map snd pairs


-- e) function stemText takes a stemmer function, a predicate and a string
--    and returns a string consisting of stemmed words, but only if the satisfy
--    the predicate p. Words that do not satisfy the predicate are discarded.
stemText :: (String -> String) -> (String -> Bool) -> String -> String
stemText f p = unwords . map f . filter p . words


{-
  4.)
-}
-- a) function centroid that takes a list of 2D points and calculates their
--    centroid. (http://en.wikipedia.org/wiki/Centroid)
type Point = (Double, Double)
centroid :: [Point] -> Point
centroid []     = error "Cannot calculate centroid of zero points"
centroid points = (fst psum / n, snd psum / n)
  where n    = fromIntegral $ length points
        psum = foldl1 (\(x1,y1)(x2,y2) -> (x1+x2,y1+y2)) points

-- b) function groupByDist takes a list of points to group xs and a list
--    of points to group around ys. It returns a list of tuples where the
--    first element is an element of ys and the second is a list of all
--    elements of xs that are closest to that y of any y in ys. If there
--    is no such element in xs, the second list is empty.
groupByDist :: [Point] -> [Point] -> [(Point, [Point])]
groupByDist _ []  = error "Cannot group around less than one point"
groupByDist [] ys = zip ys $ repeat []
groupByDist xs ys = map (\y->formGroup (filterNeighbour y, y)) ys
  where d (x1, y1) (x2, y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
        nearestPoint  point = snd . minimum $ zip (map (d point) ys) ys
        neighbours          = zip (map nearestPoint xs) xs
        filterNeighbour   p = filter ((p==) . fst) neighbours
        formGroup  (fN, pt) = (pt, map (snd) fN)

-- c) function cluster performs a simple version of k-means clustering.
--    It is given a set of points xs, a number of groups to cluster into
--    k and a number of iterations i. It should take the first k elements of
--    xs as the initial "centroids". In each step, calculate the new centroids
--    for every group and use those as the centroids in the next step. Stop
--    when you've exhausted the number of iterations or when the next centroids
--    are the same as the last. Return the centroids and their accompanying points.
cluster :: [Point] -> Int -> Int -> [(Point, [Point])]
cluster [] _ _                 = error "Cannot cluster for no points"
cluster xs k _ | length xs < k = error "The number of groups cannot be greater than the number of elements"
cluster xs k i                 = clusterAcc xs i (take k xs)

clusterAcc :: [Point] -> Int -> [Point] -> [(Point, [Point])]
clusterAcc xs i last_centroids | last_centroids == centroids || i == 0 = clusters
                               | otherwise                             = clusterAcc xs (i-1) centroids
                               where clusters  = groupByDist xs last_centroids
                                     centroids = map (centroid . snd) clusters



{-
  5.)

-}
-- a) function sortTracks sorts a list of strings formatted like
--    "TrackTitle TrackNo AlbumName" by track number
sortTracks :: [String] -> [String]
sortTracks =  sortBy (comparing (findTrackNo . words))

findTrackNo = head . dropWhile(isNotInteger)

isNotInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> False
  _         -> True

-- b)
numberOfPlays :: [String] -> Integer
numberOfPlays = foldl1 (+) . map findNoPlays

findNoPlays :: String -> Integer
findNoPlays =  (read) . head . dropWhile(isNotInteger) . words


{-
  6.) function doYouSpeak computes whether the string consists of
      words in the dictionary. The dictionary is provided in the first
      argument as a list of strings.
-}

doYouSpeak :: [String] -> String -> Bool
doYouSpeak _   [] = True
doYouSpeak xss xs |  maxP == [] = False
                  | otherwise   = doYouSpeak xss (drop (length maxP) xs)
                  where maxP = maxPref xs xss

-- is this word at the beginning of this str?
isHead :: String -> String -> Bool
isHead str word = word == take (length word) str

-- find longest prefix of str among prefixes
maxPref :: String -> [String] -> String
maxPref str prefixes = case prefs of
    [] -> []
    _  -> maximum $ prefs
    where prefs = filter (isHead (map toLower str)) $ (map . map) toLower prefixes

{-
  7.) function histogram takes a string and returns an ASCII-formatted histogram of
      occurring letters. Filter out non-letter characters and ignore the casing of letters.
      The output is formatted as shown in the examples given below.

      $ histogram "Smeg - head"
          *
      *  ** ** * *
      --------------------------
      abcdefghijklmnopqrstuvwxyz
-}
-- counts occurrence of the letters
countLetters :: String -> [(Int, Char)]
countLetters = map (\xs -> (length xs, head xs)) . group . sort . filter (isAlpha) . map toLower

-- put letters of alphabet on appropriate positions
putLetters :: [Char] -> String
putLetters xs = map (\x -> if elem x xs then '*' else  ' ') ['a'..'z']

histogram :: String -> IO ()
histogram ""  = do
    putStrLn $ map (\_ -> '-') ['a'..'z']
    putStrLn ['a'..'z']

histogram str = do
    let max = maximum . map fst $ countLetters str
    let letterOcc = countLetters str
    mapM_ (\x -> putStrLn $ putLetters . map snd $ filter ((>= x) . fst) letterOcc) [max, (max - 1)..1]
    putStrLn $ map (\_ -> '-') ['a'..'z']
    putStrLn ['a'..'z']

{-
  8.)
-}
-- a) Function smooth returns a smoothed version of a given function.
--    Function f(x) is smoothed by averaging the sum of f(x - dx), f(x) and
--    f(x + dx). The value dx is accepted as an additional parameter, (the Range).
type Range = Double
smooth :: Range -> (Double -> Double) -> Double -> Double
smooth dx f x = (f (x - dx) + f x + f (x + dx)) / 3

-- b) function nfold applies a given function n times.
nfold :: Int -> (a -> a) -> a -> a
nfold 0 f x = x
nfold n f x = f $ nfold (n - 1) f x

-- c) nsmooth smoothes a given function n times.
nsmooth :: Int -> Range -> (Double -> Double) -> Double -> Double
nsmooth n dx f = nfold n (smooth dx f)




