-- 1
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- 1a. rewrite using divMod
-- I must have done this wrong since the same result
-- could be achieved with just `mod` instead of `divMod`,
-- but I can't figure out how to use just `divMod` as the
-- question suggests...
tensDigit' :: Integral a => a -> a
tensDigit' = snd . (`divMod` 10) . (`div` 10)

-- 1b does the divMod version have the same type as the original?
-- yes

-- 1c now rewrite it to get the hundreds digit instead.
hunsD x = d2
  where d  = x `div` 100
        d2 = d `mod` 10

-- 2 write this function using a case and then with a guard
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True  -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b == True  = x
  | b == False = y
  | otherwise  = x

-- 3 fill in the definition
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

-- 4 experiment with pointfree.

