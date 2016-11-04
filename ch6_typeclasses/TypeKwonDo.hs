-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a /= b

-- 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a - fromInteger i
