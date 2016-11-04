-- write your own enumFromTo definitions for the types provided.

eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool True _     = [True]
eftBool False y    = False : eftBool (succ False) y

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = eft x y

eftInt :: Int -> Int -> [Int]
eftInt x y = eft x y

eftChar :: Char -> Char -> [Char]
eftChar x y = eft x y

eft :: (Ord a, Eq a, Enum a) => a -> a -> [a]
eft x y
  | x >  y = []
  | x == y = [x]
  | otherwise = x : eft (succ x) y


