```haskell
-- Define a generic fold function for traversing a tree
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f z Leaf = z
foldTree f z (Node x left right) = f x (foldTree f z left) (foldTree f z right)

-- Define a function to count the number of nodes in a tree
countNodes :: Tree a -> Int
countNodes = foldTree (\x _ acc -> acc + 1) 0

-- Define a function to calculate the sum of the values in a tree
sumValues :: Tree Int -> Int
sumValues = foldTree (+) 0

-- Define a function to find the maximum value in a tree
maxValue :: Tree Int -> Int
maxValue = foldTree (\x acc max -> max (x, acc)) 0

-- Define a function to find the minimum value in a tree
minValue :: Tree Int -> Int
minValue = foldTree (\x acc min -> min (x, acc)) 0

-- Define a function to find the height of a tree
height :: Tree a -> Int
height Leaf = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- Define a function to check if a tree is balanced
isBalanced :: Tree Int -> Bool
isBalanced Leaf = True
isBalanced (Node _ left right) = abs (height left - height right) <= 1 && isBalanced left && isBalanced right

-- Define a function to insert a new value into a tree
insert :: Int -> Tree Int -> Tree Int
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x < y    = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

-- Define a function to delete a value from a tree
delete :: Int -> Tree Int -> Tree Int
delete _ Leaf = Leaf
delete x (Node y left right)
  | x < y    = Node y (delete x left) right
  | x > y    = Node y left (delete x right)
  | otherwise = merge left right

-- Define a function to merge two trees
merge :: Tree Int -> Tree Int -> Tree Int
merge left right
  | height left > height right = balance left right
  | otherwise                 = balance right left

-- Define a function to balance a tree
balance :: Tree Int -> Tree Int -> Tree Int
balance left right
  | height left > height right + 1 =
    case left of
      Node y ll lr ->
        case lr of
          Node z lrr rrr -> Node z (balance ll lrr) (balance lr rrr)
          _             -> Node y (balance ll lr) right
  | height right > height left + 1 =
    case right of
      Node y llr rr ->
        case llr of
          Node z lll lrr -> Node z (balance lll llr) (balance lr rr)
          _              -> Node y left (balance llr rr)
  | otherwise = Node (rootValue left) left right

-- Define a function to get the root value of a tree
rootValue :: Tree a -> a
rootValue (Node x _ _) = x

-- Define a data type to represent a tree
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)
```

### Explanation:

This code provides a comprehensive set of functions for manipulating and analyzing binary search trees in Haskell. It includes functions for counting nodes, calculating the sum of values, finding the maximum and minimum values, determining the height, checking if a tree is balanced, inserting new values, deleting values, and merging trees. It also defines a data type to represent binary search trees and a function to extract the root value.

Here's a breakdown of the key functions:

1. `foldTree`: This is a generic fold function for traversing a tree. It takes a function that combines the current node value with the accumulated result, an initial value, and a tree, and returns the final accumulated result after traversing the tree.

2. `countNodes`: Uses `foldTree` to count the number of nodes in a tree.

3. `sumValues`: Uses `foldTree` to calculate the sum of the values in a tree.

4. `maxValue`: Uses `foldTree` to find the maximum value in a tree.

5. `minValue`: Uses `foldTree` to find the minimum value in a tree.

6. `height`: Calculates the height of a tree, which is the maximum number of edges from the root node to any leaf node.

7. `isBalanced`: Checks if a tree is balanced, meaning the heights of its left and right subtrees differ by at most 1.

8. `insert`: Inserts a new value into a tree, maintaining the binary search tree property.

9. `delete`: Deletes a value from a tree, ensuring that the resulting tree is still a valid binary search tree.

10. `merge`: Merges two binary search trees into a single balanced tree.

11. `balance`: Given two trees, it balances them by performing rotations if necessary to maintain balance.

12. `rootValue`: Extracts the value of the root node from a tree.

These functions collectively provide a comprehensive toolkit for working with binary search trees in Haskell. They can be used for various applications, such as sorting, searching, and data organization.