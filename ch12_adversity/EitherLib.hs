-------------------------------------
-- Chapter Exercises: Either library
-------------------------------------

lefts' :: [Either a b] -> [a]
lefts' = foldr getLeft []
  where getLeft (Left a) b = a : b
        getLeft _ b = b

rights' :: [Either a b] -> [b]
rights' = foldr getRight []
  where getRight (Right a) b = a : b
        getRight _ b = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = (lefts' x, rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

-- 5. This is the catamorphism
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right x) = g x

-- 6. this time user either'
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)
