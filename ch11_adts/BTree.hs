module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

-- Exercise: implement this
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- Exercise: implement these (convert Binary tree to list)
preorder :: BinaryTree a -> [a]
preorder (Node Leaf b Leaf)  = [b]
preorder (Node left b Leaf)  = b : preorder left
preorder (Node Leaf b right) = b : preorder right
preorder (Node left b right) = b : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder (Node Leaf b Leaf)  = [b]
inorder (Node left b right) = inorder left ++ [b] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder (Node Leaf b Leaf)  = [b]
postorder (Node left b right) = inorder left ++ inorder right ++ [b]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPrerder :: IO ()
testPrerder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPrerder
  testInorder
  testPostorder

-- Exercise: write foldr for the tree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b tree = foldr f b (inorder tree)
