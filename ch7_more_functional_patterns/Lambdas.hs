-- 3a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- 3b
addFive = \x -> \y -> (if x > y then y else x) + 5

-- 3c
mflip f x y = f y x
