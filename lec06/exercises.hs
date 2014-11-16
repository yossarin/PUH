-- EXERCISE 1
takeThree = take 3
dropThree = drop 3
hundredTimes = take 100 .repeat

index = zip [0..]
index' = (`zip` [0..])

divider = (`take` cycle "=")

-- EXERCISE 2

applyOnLast f xs ys = f (last xs) (last ys)

addThree :: Num a => a -> a -> a -> a 
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = applyOnLast (+) xs ys + 100

finishSentence :: String -> String
finishSentence = (++".")

applyManyTimes 0 f x = x
applyManyTimes n f x = applyManyTimes (n-1) f (f x)

applyTwice f = applyManyTimes 2 f

-- EXERCISE 3
listifyList = map (:[]) 

cutoff :: Int -> [Int] -> [Int]
cutoff = map . min 



-- 5.1.
withinInterval n m xs = filter (\x -> elem x [n..m]) xs
