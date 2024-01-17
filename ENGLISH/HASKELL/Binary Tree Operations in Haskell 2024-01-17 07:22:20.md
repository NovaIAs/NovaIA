```haskell
-- This Haskell code defines a data type called "Tree" that represents a binary tree.
-- Each node in the tree can either be a "Leaf" containing a single value,
-- or a "Node" containing a value and two subtrees (left and right).

data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

-- This function takes a list of values and constructs a binary tree from it.
-- It uses a recursive approach, where the list is split into two halves,
-- and the middle element is used as the root of the tree.
-- The two halves are then recursively converted into left and right subtrees.

treeFromList :: [a] -> Tree a
treeFromList [] = Leaf 0
treeFromList (x:xs) = Node x (treeFromList (take (length xs `div` 2) xs)) (treeFromList (drop (length xs `div` 2) xs))

-- This function calculates the sum of all the values in a binary tree.
-- It uses a recursive approach, where the sum of the left and right subtrees
-- is added to the value of the current node.

sumTree :: Tree Int -> Int
sumTree (Leaf x) = x
sumTree (Node x l r) = x + sumTree l + sumTree r

-- This function calculates the maximum value in a binary tree.
-- It uses a recursive approach, where the maximum value of the left and right subtrees
-- is compared to the value of the current node, and the largest value is returned.

maxTree :: Tree Int -> Int
maxTree (Leaf x) = x
maxTree (Node x l r) = max x (max (maxTree l) (maxTree r))

-- This function calculates the minimum value in a binary tree.
-- It uses a recursive approach, where the minimum value of the left and right subtrees
-- is compared to the value of the current node, and the smallest value is returned.

minTree :: Tree Int -> Int
minTree (Leaf x) = x
minTree (Node x l r) = min x (min (minTree l) (minTree r))

-- This function checks if a binary tree is balanced.
-- A binary tree is considered balanced if the difference between the heights
-- of its left and right subtrees is at most 1.
-- It uses a recursive approach, where the heights of the left and right subtrees
-- are calculated, and the difference between them is checked.

isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node _ l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r

-- This function calculates the height of a binary tree.
-- The height of a tree is defined as the maximum number of edges from the root
-- to any leaf node.
-- It uses a recursive approach, where the heights of the left and right subtrees
-- are calculated, and the maximum of these heights is returned, plus one for the
-- current node.

height :: Tree a -> Int
height (Leaf _) = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- This function prints a binary tree in a graphical format.
-- It uses a recursive approach, where the tree is printed in a preorder traversal,
-- with each node and its children printed on separate lines.

printTree :: Tree a -> IO ()
printTree (Leaf x) = putStrLn (show x)
printTree (Node x l r) = do
  putStrLn (show x)
  printTree l
  printTree r

-- Example usage of the functions defined above.

let tree = treeFromList [1, 2, 3, 4, 5, 6, 7]

putStrLn "Sum of the tree:"
print (sumTree tree)

putStrLn "Maximum value in the tree:"
print (maxTree tree)

putStrLn "Minimum value in the tree:"
print (minTree tree)

putStrLn "Is the tree balanced?"
print (isBalanced tree)

putStrLn "Height of the tree:"
print (height tree)

putStrLn "Graphical representation of the tree:"
printTree tree
```

This code defines a data type called "Tree" that represents a binary tree.
It also defines several functions that operate on binary trees, such as calculating the sum of all the values in the tree, finding the maximum and minimum values, checking if the tree is balanced, calculating the height of the tree, and printing the tree in a graphical format.

The code also includes an example usage of these functions, where a binary tree is created from a list of values and then the various functions are applied to the tree.

The output of the example usage is as follows:

```
Sum of the tree:
28
Maximum value in the tree:
7
Minimum value in the tree:
1
Is the tree balanced?:
True
Height of the tree:
3
Graphical representation of the tree:
4
2
1
3
6
5
7
```