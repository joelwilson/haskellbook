--------------------------
-- Exercises: Binary Tree
--------------------------

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = go $ f a
  where go (Just (x,y,z)) = Node (unfold f x) y (unfold f z)
        go Nothing = Leaf

-- 2. make a tree builder
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold inc (-1)
  where inc x
          | x == y = Nothing
          | otherwise = Just (x+1, x+1, x+1)
        y = n-1
