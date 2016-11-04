-- 1

x = (+)

f xs = w `x` 1
  where w = length xs

-- 2 identity

idx = \x -> x

-- 3 [1, 2, 3]

firstList = \(x:xs) -> x

-- 4

firstTup (a, b) = a
