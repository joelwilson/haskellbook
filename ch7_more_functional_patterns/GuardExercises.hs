-- 1 rewrite with "otherwise" at the top. what happens?
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  where y = x / 100
-- A: it always returns 'F'

-- 2 reorder the guards. what happens?
avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
  | otherwise = 'F'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y >= 0.8  = 'B'
  | y >= 0.9  = 'A'
  where y = x / 100
-- A: it really screws things up. Order matters!

-- 3. The following function returns
pal xs
    | xs == reverse xs = True
    | otherwise        = False
-- b) True when xs is a palindrome

-- 4 what types of arguments can pal take?
-- A: any list with items that implement Eq

-- 5 what is the type of the function pal?
pal :: Eq a => [a] -> Bool

-- 6 The following function returns
numbers x
    | x < 0   = -1
    | x == 0  = 0
    | x > 0   = 1
-- C: an indication of whether its argument is a positive or negative
-- number or zero.

-- 7 what types of arguments can numbers take?
-- anything which implements Num and Ord

-- 8 what is the type of the function numbers?
numbers :: (Num a, Ord a, Num b) => a -> b
