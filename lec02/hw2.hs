import Data.Char

{-
  Homework Assignment #2
-}

{-
Exercise #1-a
-}

toTitleCase s = unwords [toUpper (head w):tail w | w <- words s]
			  
{-
Exercise #1-b
-}

wToUp w = toUpper (head w):tail w

toTitleCase' s l = unwords  (map (\w -> if w `elem` l then w else wToUp w) (words s))