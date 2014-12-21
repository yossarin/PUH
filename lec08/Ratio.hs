{-
 9.) The floating point number representation only approximates real numbers.
     There are cases where we wish to avoid these inaccuracies, and for this 
     purpose we will define our own exact rational number type and its 
     associated operations.
-}
module Ratio
  ( Ratio
  , (%)
  ) where

-- a) type Ratio is representing a rational number as a ratio of two Integers
data Ratio = Frac Integer Integer | Trunc Integer  

-- b) the operator (%) returns a Ratio given two values convertible to Integer
infixl 5 %

(%) :: (Integral a, Integral b) => a -> b  -> Ratio
_ % 0 = error "Division by zero"
0 % _ = Trunc 0
x % 1 = Trunc (toInteger x)
x % y = Frac (toInteger x) (toInteger y)

-- c) instance Eq Ratio
instance Eq Ratio where
  (Trunc x) == (Trunc y)   = x == y
  (Frac x y) == (Frac z w) = x == z && y == w

-- d) instance Ord Ratio
instance Ord Ratio where
  compare (Trunc x) (Trunc y)   = compare x y
  compare (Trunc x) (Frac z w)  = compare (x*w) z
  compare (Frac x y) (Trunc z)  = compare x (y*z)
  compare (Frac x y) (Frac z w) = compare (x*w) (y*z)

-- f) instance Show Ratio
instance Show Ratio where
  show (Trunc x)  = show x
  show (Frac x y) = show (x `div` gCd) ++ " % " ++ show (y `div` gCd)
    where gCd = gcd x y
