dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- 1. write out the steps for reducing `dividedBy 15 2`
-- dividedBy 15 2
-- go 15 2 0
--  (skip first b/c 15 < 2 == False)
--  go (15-2) 2 (0+1)
--  go 13 2 1
--    (skip first b/c 13 < 2 == False)
--    go (13-2) 2 (1+1)
--    go 11 2 2
--      (skip first b/c 11 < 2 == False)
--      go (11-2) 2 (2+1)
--      go 9 2 3
--        (skip first b/c 9 < 2 == False)
--        go (9-2)  2 (3+1)
--        go 7 2 4
--          (skip first b/c 7 < 2 == False)
--          go (7-2) 2 (4+1)
--          go 5 2 5
--            (skip first b/c 5 < 2 == False)
--            go (5-2) 2 (5+1)
--            go 3 2 6
--              (skip first b/c 3 < 2 == False)
--              go (3-2) 2 (6+1)
--              go 1 2 7
--                (take first b/c 1 < 2 == True)
--                (7, 1)

-- 2. Write a function that recursively sums all nums from 1 to n,
-- n being the argument. So if n = 5, add 1 + 2 + 3 + 4 + 5 to get 15.
sumNums :: (Eq a, Num a) => a -> a
sumNums 1 = 1
sumNums n = n + sumNums (n-1)
-- sumNums 5
--  5 + sumNums (5-1)
--  5 + 4 + sumNums (4-1)
--  5 + 4 + 3 + sumNums (3-1)
--  5 + 4 + 3 + 2 + sumNums (2-1)
--  5 + 4 + 3 + 2 + 1
--  15

-- 3. Write a function that multiplies two integral numbers using
-- recursive summation. Type is (Integral a) => a -> a -> a
multiplyWithSum :: (Integral a) => a -> a -> a
multiplyWithSum 1 y = y
multiplyWithSum x y = (y+) . multiplyWithSum (x-1) $ y

-- Fix Divided By
-- Should handle negative numbers (see div for example)
-- div   10  (-2) = -5
-- div (-10) (-2) =  5
-- div (-10)   2  = -5
-- should handle dividing by zero.
data DividedResult =
    Result (Integer, Integer)
  | DividedByZero deriving (Show)

dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num denom
  | denom == 0             = DividedByZero
  | (num < 0 && denom < 0) = Result result
  | (num < 0 || denom < 0) = Result (negate (fst result), snd result)
  | otherwise              = Result result
  where go n   d count
         | n < d  = (count, toInteger n)
         | otherwise = go (n - d) d (count + 1)
        result = go (abs num) (abs denom) 0

-- McCarthy 91 function
mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11
