```haskell
-- A complex and differentiated Haskell code

-- Import necessary libraries
import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad

-- Define some data structures
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

-- Define some helper functions
treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + (treeSize left) + (treeSize right)

treeLeaves :: Tree a -> [a]
treeLeaves Empty = []
treeLeaves (Node x left right) = x : (treeLeaves left) ++ (treeLeaves right)

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Empty = Node x Empty Empty
treeInsert x (Node y left right)
  | x < y     = Node y (treeInsert x left) right
  | otherwise = Node y left (treeInsert x right)

treeDelete :: Ord a => a -> Tree a -> Tree a
treeDelete x Empty = Empty
treeDelete x (Node y left right)
  | x == y    = merge left right
  | x < y     = Node y (treeDelete x left) right
  | otherwise = Node y left (treeDelete x right)
  where
    merge left right =
      case right of
        Empty -> left
        Node y _ _ -> Node y left (treeDelete y right)

-- Define the main function
main :: IO ()
main = do
  -- Create a binary search tree
  let tree = fromList [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]

  -- Print the tree
  print tree

  -- Print the height of the tree
  print (treeHeight tree)

  -- Print the size of the tree
  print (treeSize tree)

  -- Print the leaves of the tree
  print (treeLeaves tree)

  -- Insert a new element into the tree
  let newTree = treeInsert 20 tree

  -- Print the new tree
  print newTree

  -- Delete an element from the tree
  let deletedTree = treeDelete 15 newTree

  -- Print the deleted tree
  print deletedTree

-- Helper function to convert a list of elements into a binary search tree
fromList :: Ord a => [a] -> Tree a
fromList [] = Empty
fromList (x:xs) = treeInsert x (fromList xs)
```

Explanation:

This Haskell code defines a data structure called `Tree` which represents a binary search tree. It also defines a number of helper functions to work with binary search trees, such as `treeHeight`, `treeSize`, `treeLeaves`, `treeInsert`, and `treeDelete`.

The main function first creates a binary search tree from a list of numbers. It then prints the tree, its height, its size, and its leaves. It then inserts a new element into the tree and prints the new tree. Finally, it deletes an element from the tree and prints the deleted tree.

This code demonstrates a number of advanced Haskell features, such as pattern matching, recursion, and higher-order functions. It is a good example of how to use Haskell to write concise and expressive code.