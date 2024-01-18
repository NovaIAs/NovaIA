```haskell
import Data.List (nub, sort)
import Data.Tuple (swap)
import Data.Char (toUpper)

-- Define a binary tree data type
data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- Function to insert a value into a binary tree
insert :: Ord a => a -> Tree a -> Tree a
insert x (Leaf y) = Node (Leaf y) x (Leaf y)
insert x (Node l y r)
  | x < y = Node (insert x l) y r
  | x > y = Node l y (insert x r)
  | otherwise = Node l y r

-- Function to delete a value from a binary tree
delete :: Ord a => a -> Tree a -> Tree a
delete x (Leaf y) = Leaf y
delete x (Node l y r)
  | x < y = Node (delete x l) y r
  | x > y = Node l y (delete x r)
  | otherwise = case (l, r) of
    (Leaf _, Leaf _) -> Leaf y
    _ -> let (zr, y') = findMinAndRemoveMax r in
          Node l y' zr

-- Function to find the minimum value in a binary tree
findMin :: Ord a => Tree a -> a
findMin (Leaf x) = x
findMin (Node l x r) = findMin l

-- Function to find the maximum value in a binary tree
findMax :: Ord a => Tree a -> a
findMax (Leaf x) = x
findMax (Node l x r) = findMax r

-- Function to find the successor of a value in a binary tree
successor :: Ord a => a -> Tree a -> a
successor x (Node l y r)
  | x < y = findMin r
  | x > y = successor x r
  | otherwise = y

-- Function to find the predecessor of a value in a binary tree
predecessor :: Ord a => a -> Tree a -> a
predecessor x (Node l y r)
  | x < y = predecessor x l
  | x > y = findMax l
  | otherwise = y

-- Function to find all the paths from the root to a given value in a binary tree
pathsTo :: Ord a => a -> Tree a -> [[a]]
pathsTo x (Leaf y) = if x == y then [[]] else []
pathsTo x (Node l y r)
  | x < y = map (y :) (pathsTo x l)
  | x > y = map (y :) (pathsTo x r)
  | otherwise = [] ++ map (y :) (pathsTo x l) ++ map (y :) (pathsTo x r)

-- Function to find the longest path from the root to a leaf in a binary tree
longestPath :: Tree a -> Int
longestPath (Leaf _) = 1
longestPath (Node l _ r) = max (1 + longestPath l) (1 + longestPath r)

-- Function to count the number of nodes in a binary tree
countNodes :: Tree a -> Int
countNodes (Leaf _) = 1
countNodes (Node l _ r) = 1 + countNodes l + countNodes r

-- Function to check if a binary tree is balanced
isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node l _ r) = abs (longestPath l - longestPath r) <= 1 && isBalanced l && isBalanced r

-- Function to find the height of a binary tree
height :: Tree a -> Int
height (Leaf _) = 0
height (Node l _ r) = 1 + max (height l) (height r)

-- Function to find the width of a binary tree at a given level
widthAtLevel :: Int -> Tree a -> Int
widthAtLevel _ (Leaf _) = 0
widthAtLevel level (Node l _ r)
  | level == 0 = 1
  | otherwise = widthAtLevel (level - 1) l + widthAtLevel (level - 1) r

-- Function to find the diameter of a binary tree
diameter :: Tree a -> Int
diameter (Leaf _) = 0
diameter (Node l _ r) = max (diameter l) (diameter r) (longestPath l + longestPath r)

-- Function to find all the leaf nodes in a binary tree
leaves :: Tree a -> [a]
leaves (Leaf x) = [x]
leaves (Node l _ r) = leaves l ++ leaves r

-- Function to find all the internal nodes in a binary tree
internalNodes :: Tree a -> [a]
internalNodes (Leaf _) = []
internalNodes (Node l x r) = x : internalNodes l ++ internalNodes r

-- Function to find the lowest common ancestor of two values in a binary tree
lca :: Ord a => a -> a -> Tree a -> a
lca x y (Leaf z) = if x == z || y == z then z else error "Values not found in tree"
lca x y (Node l z r)
  | x < z && y < z = lca x y l
  | x > z && y > z = lca x y r
  | otherwise = z

-- Function to find the distance between two values in a binary tree
distance :: Ord a => a -> a -> Tree a -> Int
distance x y (Leaf z) = if x == z || y == z then 0 else error "Values not found in tree"
distance x y (Node l z r)
  | x < z && y < z = distance x y l + 1
  | x > z && y > z = distance x y r + 1
  | otherwise = distance x z l + distance z y r + 1

-- Function to print a binary tree in a graphical way
printTree :: Tree a -> IO ()
printTree (Leaf x) = putStrLn (show x)
printTree (Node l x r) = do
  putStrLn (show x)
  putStrLn (replicate (longestPath l) ' ') ++ '|'
  printTree l
  putStrLn (replicate (longestPath l) ' ') ++ '|'
  printTree r

-- Function to create a binary tree from a list of values
fromList :: Ord a => [a] -> Tree a
fromList [] = Leaf (error "Empty list")
fromList (x:xs) = insert x (fromList xs)

-- Function to test the binary tree functions
testTree = fromList [10, 5, 15, 2, 7, 12, 20]

main :: IO ()
putStrLn "Minimum value in the tree:"
print (findMin testTree)

putStrLn "Maximum value in the tree:"
print (findMax testTree)

putStrLn "Successor of 7 in the tree:"
print (successor 7 testTree)

putStrLn "Predecessor of 15 in the tree:"
print (predecessor 15 testTree)

putStrLn "Paths from the root to 12 in the tree:"
print (pathsTo 12 testTree)

putStrLn "Longest path from the root to a leaf in the tree:"
print (longestPath testTree)

putStrLn "Number of nodes in the tree:"
print (countNodes testTree)

putStrLn "Is the tree balanced?"
print (isBalanced testTree)

putStrLn "Height of the tree:"
print (height testTree)

putStrLn "Width of the tree at level 2:"
print (widthAtLevel 2 testTree)

putStrLn "Diameter of the tree:"
print (diameter testTree)

putStrLn "Leaf nodes in the tree:"
print (leaves testTree)

putStrLn "Internal nodes in the tree:"
print (internalNodes testTree)

putStrLn "Lowest common ancestor of 2 and 15 in the tree:"
print (lca 2 15 testTree)

putStrLn "Distance between 2 and 15 in the tree:"
print (distance 2 15 testTree)

putStrLn "Graphical representation of the tree:"
printTree testTree
```

This code implements a binary tree data type in Haskell. A binary tree is a hierarchical data structure in which each node has a maximum of two child nodes. The code defines the data type `Tree a`, where `a` is the type of the values stored in the tree. The data type has two constructors: `Leaf` and `Node`. A `Leaf` node is a leaf node, which has no child nodes. A `Node` node is an internal node, which has a left child node, a value, and a right child node.

The code also defines several functions for manipulating binary trees. These functions include:

* `insert`: Inserts a value into a binary tree.
* `delete`: Deletes a value from a binary tree.
* `findMin`: Finds the minimum value in a binary tree.
* `findMax`: Finds the maximum value in a binary tree.
* `successor`: Finds the successor of a value in a binary tree.
* `predecessor`: Finds the predecessor of a value in a binary tree.
* `pathsTo`: Finds all the paths from the root to a given value in a binary tree.
* `longestPath`: Finds the longest path from the root to a leaf in a binary tree.
* `countNodes`: Counts the number of nodes in a binary tree.
* `isBalanced`: Checks if a binary tree is balanced.
* `height`: Finds the height of a binary tree.
* `widthAtLevel`: Finds the width of a binary tree at a given level.
* `diameter`: Finds the diameter of a binary tree.
* `leaves`: Finds all the leaf nodes in a binary tree.
* `internalNodes`: Finds all the internal nodes in a binary tree.
* `lca`: Finds the lowest common ancestor of two values in a binary tree.
* `distance`: Finds the distance between two values in a binary tree.
* `printTree`: Prints a binary tree in a graphical way.
* `fromList`: Creates a binary tree from a list of values.

The code also includes a `main` function, which tests the binary tree functions on a sample binary tree.