-- 1

x = (+)

f xs = w `x` 1
  where w = length xs

-- 2 identity

idx = \x -> x

-- 3 [1, 2, 3]

first = \(x:xs) -> x
