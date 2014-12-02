import Data.List

data Tricolor = Red | Green | Blue deriving Show

warmColor :: Tricolor -> Bool
warmColor Red = True
warmColor   _ = False

myColor = Red

data Shape =
    Circle Double Double Double
  | Rectangle Double Double Double Double
  deriving Show

isCircle :: Shape -> Bool
isCircle (Circle _ _ _) = True
isCircle _              = False

myCircle = Circle 5 5 10
mcRectangle = Rectangle 10 10 100 200
unitCircle x y = Circle x y 1

area :: Shape -> Double
area (Circle _ _ r)          = r ^ 2 * pi
area (Rectangle x1 y1 x2 y2) = (abs x1-x2)*(abs y1-y2)

totalArea :: [Shape] -> Double
totalArea = sum . map area

-- EXERCISE 1
data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y

data Point = Point Double Double
  deriving Show

data Shape2 = Circle2 Point Double | Rectangle2 Point Point
  deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point xp yp) r) = Circle2 (Point (xp+x) (yp+y)) r
translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) = Rectangle2 (Point (x1+x) (y1+y)) (Point (x2+x) (y2+y))

data Level = Undergrad | Master | Phd deriving (Eq, Show)
data Student = Student 
  { firstName :: String
  , lastName  :: String
  , studentId :: String
  , level     :: Level
  , avrege    :: Double }
  deriving Show

-- EXERCISE 2
improvStudent :: Student -> Student
improvStudent s = s {avrege = (avrege s + 1)}


