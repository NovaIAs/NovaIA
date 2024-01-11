```haskell
-- This Haskell code implements a generic binary tree data structure and
-- provides a variety of functions for manipulating and traversing trees.

-- Define the data type for binary trees.
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Function to create a leaf node.
leaf :: a -> Tree a
leaf x = Leaf

-- Function to create a node with a value and two subtrees.
node :: a -> Tree a -> Tree a -> Tree a
node x left right = Node x left right

-- Function to check if a tree is empty.
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

-- Function to get the value of a node.
getValue :: Tree a -> a
getValue (Node x _ _) = x

-- Function to get the left subtree of a node.
getLeft :: Tree a -> Tree a
getLeft (Node _ left _) = left

-- Function to get the right subtree of a node.
getRight :: Tree a -> Tree a
getRight (Node _ _ right) = right

-- Function to insert a new value into a tree.
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = node x Leaf Leaf
insert x (Node y left right)
  | x < y = node y (insert x left) right
  | otherwise = node y left (insert x right)

-- Function to delete a value from a tree.
delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Node y left right)
  | x < y = node y (delete x left) right
  | x > y = node y left (delete x right)
  | otherwise = merge left right

-- Function to merge two trees.
merge :: Tree a -> Tree a -> Tree a
merge Leaf right = right
merge left Leaf = left
merge (Node x left1 right1) (Node y left2 right2)
  | x < y = node x (merge left1 left2) (node y right1 right2)
  | otherwise = node y (node x left1 right1) (merge left2 right2)

-- Function to find the minimum value in a tree.
findMin :: Ord a => Tree a -> a
findMin (Node x Leaf _) = x
findMin (Node _ left _) = findMin left

-- Function to find the maximum value in a tree.
findMax :: Ord a => Tree a -> a
findMax (Node x _ Leaf) = x
findMax (Node _ _ right) = findMax right

-- Function to find the height of a tree.
height :: Tree a -> Int
height Leaf = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- Function to check if a tree is balanced.
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ left right) =
  abs (height left - height right) <= 1 &&
  isBalanced left &&
  isBalanced right

-- Function to perform a preorder traversal of a tree.
preorder :: (a -> IO ()) -> Tree a -> IO ()
preorder f Leaf = return ()
preorder f (Node x left right) = do
  f x
  preorder f left
  preorder f right

-- Function to perform an inorder traversal of a tree.
inorder :: (a -> IO ()) -> Tree a -> IO ()
inorder f Leaf = return ()
inorder f (Node x left right) = do
  inorder f left
  f x
  inorder f right

-- Function to perform a postorder traversal of a tree.
postorder :: (a -> IO ()) -> Tree a -> IO ()
postorder f Leaf = return ()
postorder f (Node x left right) = do
  postorder f left
  postorder f right
  f x

-- Function to convert a tree to a list.
toList :: Tree a -> [a]
toList Leaf = []
toList (Node x left right) = toList left ++ [x] ++ toList right

-- Function to convert a list to a tree.
fromList :: Ord a => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = insert x (fromList xs)

-- Function to print a tree in a graphical format.
printTree :: (Show a) => Tree a -> String
printTree Leaf = ""
printTree (Node x left right) =
  unlines [
    "  " ++ printTree left,
    show x,
    "  " ++ printTree right
  ]

-- Example usage of the binary tree functions.
main :: IO ()
main = do
  -- Create a binary tree.
  let tree = fromList [10, 5, 15, 2, 7, 12, 20]

  -- Print the tree in a graphical format.
  putStrLn $ printTree tree

  -- Insert a new value into the tree.
  let newTree = insert 8 tree

  -- Print the new tree in a graphical format.
  putStrLn $ printTree newTree

  -- Delete a value from the tree.
  let deletedTree = delete 15 newTree

  -- Print the deleted tree in a graphical format.
  putStrLn $ printTree deletedTree

  -- Find the minimum value in the tree.
  let minValue = findMin deletedTree
  putStrLn $ "Minimum value: " ++ show minValue

  -- Find the maximum value in the tree.
  let maxValue = findMax deletedTree
  putStrLn $ "Maximum value: " ++ show maxValue

  -- Find the height of the tree.
  let treeHeight = height deletedTree
  putStrLn $ "Height of the tree: " ++ show treeHeight

  -- Check if the tree is balanced.
  let isBalancedTree = isBalanced deletedTree
  putStrLn $ "Is the tree balanced? " ++ show isBalancedTree

  -- Perform a preorder traversal of the tree.
  putStrLn "Preorder traversal:"
  preorder print deletedTree

  -- Perform an inorder traversal of the tree.
  putStrLn "Inorder traversal:"
  inorder print deletedTree

  -- Perform a postorder traversal of the tree.
  putStrLn "Postorder traversal:"
  postorder print deletedTree

  -- Convert the tree to a list.
  let treeList = toList deletedTree
  putStrLn $ "Tree converted to a list: " ++ show treeList

  -- Convert the list back to a tree.
  let newTreeFromList = fromList treeList
  putStrLn $ "New tree created from the list:"
  printTree newTreeFromList
```

This code provides a comprehensive implementation of a binary tree data structure in Haskell, along with a variety of functions for manipulating and traversing trees. The code includes functions for creating and deleting nodes, inserting and deleting values, finding the minimum and maximum values, calculating the height, checking for balance, and performing preorder, inorder, and postorder traversals. Additionally, the code can convert a tree to a list and back, and provides a function for printing the tree in a graphical format. This code demonstrates the versatility and power of Haskell for implementing complex data structures and algorithms.