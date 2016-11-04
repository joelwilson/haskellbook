ex3 :: [a] -> Int -> a
ex3 xs i = head xs

ex4 :: (a, b) -> a
ex4 (x, y) = x

-- Determint the type

-- 2
-- x = 5
-- y = x + 5
-- w = y * 10

-- 3
-- x = 5
-- y = x + 5
-- z y = y * 10

-- 4
-- x = 5
-- y = x + 5
-- f = 4 / y

-- 5
-- x = "Julie"
-- y = " <3 "
-- z = "Haskell"
-- f = x ++ y ++ z

-- Does it compile?

-- 1
bigNum = (^) 5
wahoo = bigNum $ 10

-- 2
x = print
y = print "woohoo!"
z = x "hello world"

-- 3
aa = (+)
bb = 5
cc = aa 10
dd = cc 200

-- 4
aaa = 12 + bbb
bbb = 10000 * ccc
ccc = 5

-- Type variable or specific type constructor?

-- 2
data Zed
data Blah

f2 :: zed -> Zed -> Blah
f2 = undefined
--    [0]    [1]    [2]
-- Fully polymorphic (0), concrete (1 & 2)

-- 3
f3 :: Enum b => a -> b -> c
f3 = undefined
--             [0]  [1]  [2]
-- Fully polymorphic (0), constrainted polymorphic (Enum)(1),
-- fully polymorphic (2).

-- 4
type C = String
f4 :: f -> g -> C
f4 = undefined
--   [0]  [1]  [2]
-- Fully polymorphic (0), fully polymorphic (1), concrete (2)

-- Write a type signature

-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

-- 1
i :: a -> a
i x = x

-- 2
c :: a -> b -> a
c x _ = x

-- 3
c'' :: b -> a -> b
c'' x y = x

-- 4
c' :: a -> b -> b
c' x y = y

-- 5
r :: [a] -> [a]
r xs = xs

r' :: [a] -> [a]
r' (x:xs) = xs

r'' :: [a] -> [a]
r'' xs = tail xs

r''' :: [a] -> [a]
r''' xs = [xs !! 1, xs !! 3]

--- etc. (although r''' throws an exception sometimes...)

-- 6
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = \x -> f $ g x

-- 7
a :: (a -> c) -> a -> a
a _ x = x

-- 8
a' :: (a -> b) -> a -> b
a' f x = f x


