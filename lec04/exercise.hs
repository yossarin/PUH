import Data.Char
import Data.List

headHunter :: [[a]] -> a
headHunter ((x:xs):xss) = x
headHunter (_:(x:xs):xss) = x
headHunter (_:_:(x:xs):_) = x
headHunter _ = error "Error"

isWellFormed l | l == [[]] = False
               | otherwise = length (nub [length e | e <- l ])== 1

firstColumn m = [e!!0 | e <- m]

shoutOutLoud :: String -> String
shoutOutLoud s = unwords [ w!!0:w!!0:w | w <- words s]

pad (s1h:s1t) (s2h:s2t) = (toUpper s1h: (fill s1t), toUpper s2h : (fill s2t))
  where l = max (length s1t) (length s2t)
        fill s | length s == l = s
	       | otherwise = s ++ (take (l-length s) $ cycle " ")


pad' (x:xs) (y:ys) =
  let l = max (length xs) (length ys)
      fill s | length s == l = s
             | otherwise = s ++ (take (l-length s) $ cycle " ")
 
  in  ((toUpper x : (fill xs)), (toUpper y : (fill ys)))


