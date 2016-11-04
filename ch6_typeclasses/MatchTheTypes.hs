import Data.List

-- 1
i :: Num a => a
--i :: a
i = 1

-- 2
-- f :: Num a => a
f :: Float
f = 1.0
-- f :: Num a => a does not work b/c 1.0 is a member of Fractional and while
-- Num is a superclass of Fractional, it does not imply Fractional. If the
-- signature was made to be f :: Fractional a => a and f = 1 then it works because
-- Fractional implies Num, which is what 1 is.

f2 :: Fractional a => a
f2 = 1

-- 3
-- g :: Float
g :: Fractional a => a
g = 1.0

-- 4
-- h :: Float
h :: RealFrac a => a
h = 1.0
-- works b/c RealFrac requires its instances also be part of Fractional.
-- Also it can be seen from :info that Double implements RealFrac.

-- 5
--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- 6
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- 7
myX = 1 :: Int

-- sigmund :: a -> a
sigmund :: Int -> Int
sigmund x = myX

-- 8
--sigmund' :: Int -> Int
sigmund' :: Num a => a -> Int
sigmund' x = myX

-- 9
--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10
--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)


