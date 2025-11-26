data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

expectedTree :: BinaryTree Integer
expectedTree = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

actualTree :: BinaryTree Integer
actualTree = mapTree (+ 1) testTree

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left ++ preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ a : inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left ++ postorder right) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = f a $ foldTree f (foldTree f b right) left
