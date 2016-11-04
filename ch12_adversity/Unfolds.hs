------------------------------
-- Chapter Exercises: Unfolds
------------------------------

-- iterate is like a very limited
-- unfold that never ends

-- 1. write myIterate using direct recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2. write myUnfoldr using direct recursion
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go $ f b
  where go (Just (a, b')) = a : myUnfoldr f b'
        go Nothing = []
-- BOOM!

-- 3. rewrite myIterate using myUnfoldr
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))
