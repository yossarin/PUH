takeThree = take 3
dropThree = drop 3
hundredTimes = take 100 .repeat

index = zip [0..]
index' = (`zip` [0..])

divider = (`take` cycle "=")

listifyList = map (:[]) 

cutoff :: Int -> [Int] -> [Int]
cutoff = map . min 



-- 5.1.
withinInterval n m xs = filter (\x -> elem x [n..m]) xs
