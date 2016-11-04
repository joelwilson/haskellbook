module Exercises where

import Data.Char

-- 2
filterUpper :: String -> String
filterUpper = filter isUpper

-- 3
firstUpper :: String -> String
firstUpper "" = ""
firstUpper x  = toUpper (head x) : (tail x)

-- 4
allUpper :: String -> String
allUpper "" = ""
allUpper x  = toUpper (head x) : allUpper (tail x)

-- Standard functions
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd xs

-- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem x []     = False
myElem x (y:ys) = x == y || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

-- 4
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5
squish :: [[a]] -> [a]
squish []          = []
squish ([]:xs)     = squish xs
squish ((y:ys):xs) = y : squish ([ys] ++ xs)

-- 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
  where go f [] y = y
        go f (x:xs) y
          | f x y == GT = go f xs x
          | otherwise   = go f xs y

-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f xs x
  where go f [] y = y
        go f (x:xs) y
          | f x y == LT = go f xs x
          | otherwise   = go f xs y

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
