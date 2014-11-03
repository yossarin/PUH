


{-
  1.) My own version of Data.List.Intercalate.
-}

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [x]     = x
intercalate' e (x:xss) = x ++ e ++ (intercalate' e xss)


{-
  2.) Following functions work with discrete random variables.
-}
type Probability = Double
type DiscreteRandVar = [(Int, Probability)]
x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- (a) explicitly recursive function mean and an accumulator-style recursive
--     function mean' that calculate the mean of a discrete random variable

mean :: DiscreteRandVar -> Double
mean [(x, p)]    = fromIntegral x * p
mean ((x, p):xs) = fromIntegral x * p + mean xs

meanHelp' :: DiscreteRandVar -> Double
meanHelp' [] n          = n
meanHelp' ((x, p):xs) n = mean' xs (fromIntegral x * p + n)

mean' :: DiscreteRandVar -> Double
mean' x = meanHelp' x 0