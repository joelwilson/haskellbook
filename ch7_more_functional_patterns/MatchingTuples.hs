module TupleFunctions where

-- These have to be the same type because
-- (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

-- exercises: variety pack

-- 1 given:
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- a) what is the type of k?
--    (a, b) -> a
-- b) what is the type of k2? Is it the same type as k1 or k3?
--    It is type [Char]. This is the same as neither k1 nor k3.
-- c) Of k1, k2, k3, which will return the number 3 as the result?
--    k1 and k3 will.
