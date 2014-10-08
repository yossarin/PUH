{-
  Homework Assignment #1
  Vlatko Klabucar
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
-}

ord3 x y z = 
  if x <= y && x <= z then
    if y <= z then [x,y,z] else [x,z,y]  -- ok, x is the smallest
  else 
  if y <= x && y <= z then
    if x <= z then [y,x,z] else [y,z,x]  -- ok, y is the smallest
  else 
  if x <= y then [z,x,y] else [z,y,x]    -- ok, z is the smallest
  