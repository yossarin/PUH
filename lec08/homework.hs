import Prelude hiding (Left, Right, Up, Down)

type Position    = (Integer, Integer)
data Orientation = Left | Up | Right | Down deriving (Eq, Show, Enum)
data TurnDir     = CW | CCW deriving (Eq, Show)

data Turtle = Turtle 
  { orientation :: Orientation
  , position    :: Position
  } deriving Show

newTurtle :: Turtle
newTurtle = Turtle Up (0,0)

move :: Integer -> Turtle -> Turtle
move x _      | x < 0 =  error "Turtles cannot move backwards"
move x turtle = case o of
  Left  -> Turtle o (fst p - x, snd p)
  Right -> Turtle o (fst p + x, snd p)
  Up    -> Turtle o (fst p, snd p + x)
  Down  -> Turtle o (fst p, snd p - x)
  where o = orientation turtle
        p = position turtle

turn :: TurnDir -> Turtle -> Turtle
turn dir turtle = case (o, dir) of
  (Left, CCW) -> Turtle Down p
  (Down,  CW) -> Turtle Left p
  (_,    CCW) -> Turtle (pred o) p
  (_,     CW) -> Turtle (succ o) p
  where o = orientation turtle
        p = position turtle

runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle []     t = t
runTurtle (f:[]) t = f t
runTurtle (f:fs) t = runTurtle fs $ f t 

