import Data.Char
import Data.List

x = 2

inc x = x + 1

d2n x y = x * 10 + y

y = inc 2
z = d2n 4 2

w = 2 `div` 1

name = "Tralala "

letter = 'c'

s = "lala" ++ "   Vesela druzina" ++ "   lalal" ++ "d"

n1 = length s

conDec x = if x > 0 then x - 1 else x

foo x = (if even x then x*2 else 2) +1

foo' x = if even x then x*2 else 2 +1

bigNUmber x = if x >= 1000 then True else False

bigNumber' x = x >= 1000

merge s1 s2 = s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2

merge3 s1 s2
	| s1 < s2 = s1 ++ " is "++s2
	| otherwise = s1 ++ " is not " ++ s2
	
grade score | score < 50 = 1
            | score < 63 = 2
			| score < 76 = 3
			| score < 89 = 4
			| otherwise  = 5
			
showSalary amount bonus 
	| bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
	| otherwise = "Salary is " ++ show amount
	
	
cancat2 s1 s2 = s1 ++ s2

concat3 s1 s2 s3 = if (length s2) < 2 then s1 ++ s3 else s1 ++ s2 ++ s3