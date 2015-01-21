import Data.List
import Control.Monad

data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person = Person {
    idNumber :: String,
    forename :: String,
    surname  :: String,
    sex      :: Sex,
    age      :: Int,
    partner  :: Maybe Person,
    children :: [Person]} deriving (Show,Read,Ord,Eq)

data Person2 = Person2 {
    personId2 :: String,
    forename2 :: String,
    surname2  :: String,
    sex2      :: Sex,
    mother2   :: Maybe Person2,
    father2   :: Maybe Person2,
    partner2  :: Maybe Person2,
    children2 :: [Person2]} deriving (Show,Read,Eq,Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane =  Person2 "13" "Jane" "Doe" Female (Just ann) Nothing (Just john) []
ann = Person2 "132" "Ann" "Doe" Female Nothing Nothing Nothing [jane]

partnersMother :: Person2 -> Maybe Person2
partnersMother p = case partner2 p of
    Just p' -> mother2 p'
    Nothing -> Nothing

--sister :: Person2 -> Maybe Person2 
--sister p = case 

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord,Eq)

-- EXERCISE 2
listHead :: MyList a -> Maybe a
listHead (Cons x _) = Just x
listHead Empty      = Nothing

listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty         = Empty
listMap f (x `Cons` xs) = f x `Cons` listMap f xs

-- EXERCISE 3

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)
intTree :: Tree Int
intTree = Node 1 (Node 2 Null Null) (Node 3 Null Null)

treeMax :: Ord a => Tree a -> a
treeMax Null = error "Tree is empty :("
treeMax t    = maximum $ elems t
    where elems (Node x l r) = [x]++ (elems l) ++ (elems r)
          elems Null         = []
