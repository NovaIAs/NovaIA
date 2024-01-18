```haskell
import Data.List (tails)

-- 1. Define a data type to represent a binary tree.
data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- 2. Define a function to create a binary tree from a list of values.
treeFromList :: [a] -> Tree a
treeFromList [] = Leaf 42 -- Default value for empty list
treeFromList (x:xs) = Node (treeFromList xs) x (treeFromList (tail xs))

-- 3. Define a function to find all paths from the root to a leaf in a binary tree.
paths :: Tree a -> [[a]]
paths (Leaf x) = [[x]]
paths (Node left x right) = paths left ++ map (x:) (paths right)

-- 4. Define a function to find all unique paths in a binary tree.
uniquePaths :: Tree a -> [[a]]
uniquePaths tree = nub (paths tree)

-- 5. Define a function to find the longest path in a binary tree.
longestPath :: Tree a -> Int
longestPath (Leaf _) = 1
longestPath (Node left _ right) = 1 + max (longestPath left) (longestPath right)

-- 6. Define a function to find the shortest path in a binary tree.
shortestPath :: Tree a -> Int
shortestPath (Leaf _) = 1
shortestPath (Node left _ right) = 1 + min (shortestPath left) (shortestPath right)

-- 7. Define a function to find the number of leaves in a binary tree.
numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node left _ right) = numLeaves left + numLeaves right

-- 8. Define a function to find the number of nodes in a binary tree.
numNodes :: Tree a -> Int
numNodes (Leaf _) = 1
numNodes (Node left _ right) = 1 + numNodes left + numNodes right

-- 9. Define a function to find the height of a binary tree.
height :: Tree a -> Int
height (Leaf _) = 0
height (Node left _ right) = 1 + max (height left) (height right)

-- 10. Define a function to print a binary tree using ASCII art.
printTree :: Tree a -> String
printTree (Leaf x) = show x
printTree (Node left x right) = unlines (tails (show x : printTree left : printTree right))

-- Example usage:
tree :: Tree Int
tree = treeFromList [1, 2, 3, 4, 5, 6, 7, 8, 9]

main :: IO ()
main = do
  putStrLn "Paths:"
  print (paths tree)

  putStrLn "Unique paths:"
  print (uniquePaths tree)

  putStrLn "Longest path:"
  print (longestPath tree)

  putStrLn "Shortest path:"
  print (shortestPath tree)

  putStrLn "Number of leaves:"
  print (numLeaves tree)

  putStrLn "Number of nodes:"
  print (numNodes tree)

  putStrLn "Height:"
  print (height tree)

  putStrLn "Print tree:"
  print (printTree tree)
```

Explanation:

1. The `Tree` data type represents a binary tree. It can either be a `Leaf` containing a single value or a `Node` with a left and right subtree and a value.
2. The `treeFromList` function creates a binary tree from a list of values. It uses a recursive algorithm to build the tree.
3. The `paths` function finds all paths from the root to a leaf in a binary tree. It uses a recursive algorithm to explore all possible paths.
4. The `uniquePaths` function finds all unique paths in a binary tree. It uses the `nub` function from the `Data.List` module to remove duplicate paths.
5. The `longestPath` function finds the longest path in a binary tree. It uses a recursive algorithm to calculate the length of the longest path.
6. The `shortestPath` function finds the shortest path in a binary tree. It uses a recursive algorithm to calculate the length of the shortest path.
7. The `numLeaves` function finds the number of leaves in a binary tree. It uses a recursive algorithm to count the number of leaves.
8. The `numNodes` function finds the number of nodes in a binary tree. It uses a recursive algorithm to count the number of nodes.
9. The `height` function finds the height of a binary tree. It uses a recursive algorithm to calculate the height of the tree.
10. The `printTree` function prints a binary tree using ASCII art. It uses a recursive algorithm to generate the ASCII art representation of the tree.

The example usage at the end of the code creates a binary tree from a list of integers and then calls the various functions to compute different properties of the tree. The results are then printed to the console.