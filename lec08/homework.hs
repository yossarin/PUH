import Prelude hiding (Left, Right, Up, Down)
import Data.Maybe (fromJust, isJust)
{-
 1.) small domain-specific language for enslaving turtles. 
-}

type Position    = (Integer, Integer)
data Orientation = Left | Up | Right | Down deriving (Eq, Show, Enum)
data TurnDir     = CW | CCW deriving (Eq, Show)

-- (a,b,c)
data Turtle = Turtle 
  { orientation :: Orientation
  , position    :: Position
  } deriving Show

-- (d)
newTurtle :: Turtle
newTurtle = Turtle Up (0,0)

-- (e)  function moven takes an Integer and  Turtle and moves the
--      turtle a given amount in the direction it is currently facing.
move :: Integer -> Turtle -> Turtle
move x _      | x < 0 =  error "Turtles cannot move backwards"
move x turtle = case o of
  Left  -> Turtle o (fst p - x, snd p)
  Right -> Turtle o (fst p + x, snd p)
  Up    -> Turtle o (fst p, snd p + x)
  Down  -> Turtle o (fst p, snd p - x)
  where o = orientation turtle
        p = position turtle

-- (f) function turn takes a TurnDir and a Turtle and changes the
--     turtle's position accordingly.
turn :: TurnDir -> Turtle -> Turtle
turn dir turtle = case (o, dir) of
  (Left, CCW) -> Turtle Down p
  (Down,  CW) -> Turtle Left p
  (_,    CCW) -> Turtle (pred o) p
  (_,     CW) -> Turtle (succ o) p
  where o = orientation turtle
        p = position turtle

-- (g) a function runTurtle that enables us to chain our commands to
--     the turtle more easily
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle []     t = t
runTurtle (f:[]) t = f t
runTurtle (f:fs) t = runTurtle fs $ f t 

{-
 2.)
-}
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- (a) function treeFilter that takes a predicate and a Tree and removes
--     those subtrees that do not satisfy the given predicate 
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ Leaf = Leaf
treeFilter p (Node x tl tr) | p x       = Node x (treeFilter p tl) (treeFilter p tr)
                            | otherwise = Leaf

-- (b)  function levelMap takes some binary function and applies it to
--      the tree. The function that is being applied takes the depth of
--      the tree as the first argument. The root element's depth is 0.
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f t = levelMapAcc 0 f t
  where levelMapAcc _ _ Leaf           = Leaf
        levelMapAcc d f (Node x lt rt) = Node (f d x) (levelMapAcc (d+1) f lt) (levelMapAcc (d+1) f rt)

-- (c)  function isSubtree takes two instances of Tree and checks 
--      whether the first tree appears as part of the second.
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf Leaf                              = True
isSubtree _    Leaf                              = False
isSubtree t1 t2@(Node _ lt rt) | consistOf t2 t1 = True
                               | otherwise       = isSubtree t1 lt || isSubtree t1 rt
  where consistOf Leaf           Leaf                    = True
        consistOf _              Leaf                    = False
        consistOf Leaf           (Node _ _ _)            = False
        consistOf (Node x lx rx) (Node y ly ry) | x == y = consistOf lx ly && consistOf rx ry
                                                | otherwise = False
{-
 3.)
-}

data Date = Date 
  { day   :: Int
  , month :: Int
  , year  :: Int
  } deriving (Show, Eq)
-- fuunction return True if its argument is a leap year
isLeap :: Int -> Bool
isLeap y = y `mod` 4 == 0 && (not (y `mod` 100 == 0) || y `mod` 400 == 0)
-- (a) function date constructs a date from three integers. It is to be
--     used instead of the Date constructor to check for date validity. 
--     If the date is invalid, Nothing is returned. 
--     Otherwise, Just Date is returned.
date :: Int -> Int -> Int -> Maybe Date
date d m y = case (d, m, isLeap y) of
  (d, 2, True) -> if d `elem` [1..29] then Just (Date d 2 y) else Nothing
  (d, 2, _   ) -> if d `elem` [1..28] then Just (Date d 2 y) else Nothing
  (d, m, _   ) -> case m `elem` [1,3,5,7,8,10,12] of
                    True -> if d `elem` [1..31] then Just (Date d m y) else Nothing
                    _    -> if d `elem` [1..30] then Just (Date d m y) else Nothing

-- (b) function addDays takes a Date and an Int as a number of days
--     to add and calculates the new Date.
positive :: Date -> Int -> Date
positive (Date d m y) n = case isJust $ date (d+n) m y of
      True  -> fromJust $ date (d+n) m y
      False -> case (m, isLeap y) of
                 ( 2, True) -> fromJust $ date (d+n-29) 3 y
                 ( 2, _   ) -> fromJust $ date (d+n-28) 3 y
                 (12, _   ) -> fromJust $ date (d+n-31) 1 (y+1)
                 _         -> case m `elem` [1,3,5,7,8,10] of
                                True -> fromJust $ date (d+n-31) (m+1) y
                                _    -> fromJust $ date (d+n-30) (m+1) y

negative :: Date -> Int -> Date
negative (Date d m y) n = case isJust $ date (d+n) m y of
      True  -> fromJust $ date (d+n) m y
      False -> case (m, isLeap y) of
                 (2, True) -> fromJust $ date (d+n+29) 1 y
                 (2, _   ) -> fromJust $ date (d+n+28) 1 y
                 (1, _   ) -> fromJust $ date (d+n+31) 12 (y-1)
                 (3, True) -> fromJust $ date (d+n+29) 2 y
                 (3, _   ) -> fromJust $ date (d+n+28) 2 y
                 _         -> case m `elem` [3,5,7,8,10,12] of
                                True -> fromJust $ date (d+n+31) (m-1) y
                                _    -> fromJust $ date (d+n+30) (m-1) y

addDays :: Date -> Int -> Date
addDays dat@(Date d m y) n = if n < 0 then negative dat n else positive dat n
