import Prelude hiding (Left, Right, Up, Down)

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
