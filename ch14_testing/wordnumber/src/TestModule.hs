module TestModule where

import Data.Char (toUpper)

-- Quick Check exercise methods go here. See the
-- test/QuickCheckTests.hs for more info.

-- QC Exercise 1
half x = x / 2

-- this property should hold
halfIdentity = (*2) . half

-- QC Exercise 2
-- for any list youa pply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y,x >= y)

-- QC Exercise 3
-- Test the associative and commutatitive properties of addition
plusAssociative :: (Ord a, Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Ord a, Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

-- QC Exercise 4
-- same for multiplication
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y =
  x * y == y * x

-- QC exercise 5
quotRemLaw _ 0 = True -- why not?
quotRemLaw x y = (quot x y) * y + (rem x y) == x

divModLaw _ 0 = True -- why not?
divModLaw x y = (div x y) * y + (mod x y) == x

-- QC exercise 6
-- Is (^) associative? Commutative? Use QC to try to contradict each.
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

expCommutative x y = x ^ y == y ^ x

-- QC exercise 7
-- list reverse reverse identity
doubleReverseId xs = (reverse . reverse) xs == xs

-- QC exercise 8
-- write a property for the definition of ($)
dollarIdentity f a = (f $ a) == f a

-- QC exercise 9
-- see if these two functions are equal

-- A: no, they're backwards! [0] [1] gives [1,0] and [0,1]
foldrConsEqAppend xs ys = foldr (:) xs ys == (++) xs ys

-- A: yeah looks like they are equal.
foldrAppendEqConcat xs = foldr (++) [] xs == concat xs

-- QC exercise 10
-- A: It should have been obvious, but n is not equal to taking
-- from the list if the number of items in the list is less than n.
-- Ex: length (take 2 ['a']) == 1 (not 2)
exercise10 n [] = n == 0
exercise10 n xs = length (take n xs) == n

-- QC exercise 11
readShowId x = (read (show x)) == x

-------
-- Failure
-- Find out why thsi property fails

-- for a function
square x = x * x

-- why does this property not hold? Examine the type of sqrt.
squareIdentity = square . sqrt

-------
-- Idempotence
twice f = f . f
fourTimes = twice . twice
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

