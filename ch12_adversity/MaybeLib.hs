------------------------------------
-- Chapter Exercises: Maybe library
------------------------------------

-- 1. simple boolean checks for Maybe values
isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' _ = True

isNothing' :: Maybe a -> Bool
isNothing' = not . isJust'

-- 2. Maybe catamorphism. Can turn a Maybe into anything.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee x _ Nothing  = x

-- 3. provide a fallback value
fromMayybe :: a -> Maybe a -> a
fromMayybe _ (Just b) = b
fromMayybe a Nothing  = a

-- 4. convert between list and maybe
listToMayybe :: [a] -> Maybe a
listToMayybe [] = Nothing
listToMayybe xs = Just $ head xs

mayybeToList :: Maybe a -> [a]
mayybeToList (Just a) = [a]
mayybeToList Nothing  = []

-- 5. for when you want to drop the Nothing values
catMayybes :: [Maybe a] -> [a]
catMayybes = concatMap mayybeToList

-- 6. flipMayybe (called "sequence" later)
flipMayybe :: [Maybe a] -> Maybe [a]
flipMayybe = foldr go (Just [])
  where go _ Nothing = Nothing
        go Nothing _ = Nothing
        go (Just a) (Just b) = Just $ a:b
