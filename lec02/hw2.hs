import Data.Char
import Data.List
import System.Environment

{-
  Homework Assignment #2
-}

{-
Exercise #1-a
 Using built in functions words and unwords to parse and concat input, and 
 toUpper to capitalize first letter.
-}

toTitleCase s = unwords [toUpper (head w):tail w | w <- words s]

{-
Exercise #1-b
 Using lambda expression to catch occurrences of elements present in list l.
-}

wToUp w = toUpper (head w):tail w

toTitleCase' s l = unwords  (map (\w -> if w `elem` l then w else wToUp w) (words s))

{-
Exercise #2
 This function uses some of that reverse magic, as well as amazing $ function.
-}

trimN l n
  | length l < 2*n = l
  | otherwise = reverse $ drop n $ reverse $ drop n l

{-
Exercise #3
 When compiled with ghc this little program prints the contents of the file, named
 as the first command line argument, to the standard output.
-}
  
main = do
  args <- getArgs
  f <- readFile $ args!!0
  putStr f
  
{-
Exercise #4
 Zip characters with their indices and run it through the comprehension
 with appropriate constraints.
-}

onlyDivisible s n 
 | n < 1 = error "n must be positive!"
 | otherwise = [c | (c,i) <- zip s [0..], i `mod` n == 0]
 
{-
Exercise #5
 This exercise was a bit tricky, few helper functions were introduced 
 to simplify the resulting function.
   subsetsSizeOfK - returns the list of distinct subsets of desired sizes
   areTheseNotCollinear - checks if dots are collinear using linear equation 
-}

subsetsSizeOfK l k 
  | k == 0 = [[]]
  | l == [] = []
  | otherwise = ((head l):) `map` ((tail l) `subsetsSizeOfK` (k-1)) ++ (tail l) `subsetsSizeOfK` k

areTheseNotCollinear [(x,y), (x1,y1), (x2,y2)] = y /= (y2 - y1)/(x2 - x1)*(x - x1) + y1

triangleCounter l = length $ [trs | trs <- subsetsSizeOfK (nub l) 3, areTheseNotCollinear trs]


{- Exercise #6
  Too easy for this Haskell magic.
-}

reverseWords s = unwords $ reverse $ words s

{- Exercise #7
  Oh boy oh boy.
-}

isNonEmpty [] = False
isNonEmpty (_:_) = True

longerThan n xs = isNonEmpty $ drop n xs

returnShorterListFirst l1 l2 n
  | longerThan n l1 && longerThan n l2 = returnShorterListFirst l1 l2 (n+100)
  | longerThan n l1 == False = (l1,l2)
  | longerThan n l2 == False = (l2,l1)
  
isFirstListShorter l1 l2 n
 | longerThan n l1 && longerThan n l2 = isFirstListShorter l1 l2 (n+100)
 | longerThan n l1 == False = True
 | longerThan n l2 == False = False

intersectUnsafe (l1, l2) = [el | el <- l1, el `elem` l2]

differenceUnsafe (l1, l2) = [el | el <- l1, el `notElem` l2]

intersect' l1 l2
  | l1 == [] || l2 == [] = [] 
  | otherwise = intersectUnsafe $ returnShorterListFirst l1 l2 100
 
difference l1 l2
  | l1 == [] = []
  | l2 == [] = l1
  | isFirstListShorter l1 l2 100 = [el | el <- l1, el `notElem` intersect' l1 l2]
  
  