import Data.Char
import Data.List

fst' :: (a,b) -> a
fst' (x,_) = x

addVectors :: (Double, Double) -> (Double, Double) ->  (Double, Double)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

mITM :: (a,b,c) -> b
mITM (_,y,_) = y

leaves :: ((a, a), (a, a)) -> [a]
leaves ((x,y), (z,w)) = [x,y,z,w]

head' :: [a] -> a
head' [] = error "No head to behead"
head' (x:_) = x

headHunter :: [[a]] -> a
headHunter ((x:xs):xss) = x
headHunter (_:(x:xs):xss) = x
headHunter (_:_:(x:xs):_) = x
headHunter _ = error "Error"

pad (s1h:s1t) (s2h:s2t) = (toUpper s1h: (fill s1t), toUpper s2h : (fill s2t))
  where l = max (length s1t) (length s2t)
        fill s | length s == l = s
	       | otherwise = s ++ (take (l-length s) $ cycle " ")


pad' (x:xs) (y:ys) =
  let l = max (length xs) (length ys)
      fill s | length s == l = s
             | otherwise = s ++ (take (l-length s) $ cycle " ")
 
  in  ((toUpper x : (fill xs)), (toUpper y : (fill ys)))

magicNumber :: Int -> String
magicNumber x = case x of
  42 -> "Yeah!"
  _  -> "Nope, try again."
  
takePair (x,y) (_:e:_) = "The pair "++(case (x,y) of
  (1,1) -> "contains two ones "
  (1,_) -> "contains one one "
  (_,1) -> "contains one one "
  _     -> "does not contain a single one ")++
  "and the second elem of the list is " ++ show e

