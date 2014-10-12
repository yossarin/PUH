import Data.Char

{-
  Homework Assignment #1
-}


{-
Exercise #1
  The function first checks if the boundaries are defined properly,
  and then checks if the length of the string is inside the boundaries.
-}

strlenInRange str start end
  | start < 0 = error "Start boundary cannot be negative."
  | end   < 0 = error "End boundary cannot be negative."
  | length str <= end && length str >= start = True
  | otherwise = False


{-
Exercise #2
  The functions first makes sure the index is valid,
  then checks if an element of list with given index 
  is larger than provided value.
-}
 
isHereAGreater list index value
  | index < 0 || index >= length list = False
  | list!!index > value = True
  | otherwise           = False

 
{-
Exercise #3
  This function reads two lines from  the standard input,
  a sentence and a single word. All occurrences of the given 
  word in the sentence is removed and the sentence is printed out.
  This was accomplished using functions words and unwords and a 
  list comprehension.
-}

wordFilter = do
  sentence <- getLine
  word <- getLine
  putStrLn (unwords [w | w <- words sentence, w /= word])
  
{-
Exercise #4
  This function returns a sorted list of given numbers in ascending order.
  The main idea is that if we know the smallest number, then one comparison 
  between the remaining two would give the second smallest and the largest number.
  
  PLEASE GIVE ME FEEDBACK: Can this function be written better in respect to the indentation
  and style in general, looking at it I find it a bit confusing.
-}

ord3 x y z = 
  if x <= y && x <= z then
    if y <= z then [x,y,z] else [x,z,y]  -- ok, x is the smallest
  else 
  if y <= x && y <= z then
    if x <= z then [y,x,z] else [y,z,x]  -- ok, y is the smallest
  else 
  if x <= y then [z,x,y] else [z,y,x]    -- ok, z is the smallest

  
{-
Exercise #5
  Using 2-tuples to represent vectors in 2D space, as (x,y),
  the following operators are implemented:
-}

norm (x,y) = sqrt (x*x + y*y)

add (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

scalarMult (x,y) val = (x*val, y*val)

dot (x1,y1) (x2,y2) = x1*x2 + y1*y2


{-
Exercise #6
  Uses function ord located in Data.Char module imported on the top.
-}

asciiRange a b = zip [a..b] (map ord [a..b])


{-
Exercise #7
  Uses functions ord and chr located in Data.Char module 
  imported on the top.
-}

incn x str = [chr (y + x) | y <- map ord str]