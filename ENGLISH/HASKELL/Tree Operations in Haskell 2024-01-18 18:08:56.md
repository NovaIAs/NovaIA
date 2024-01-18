```haskell
module ComplexHaskell where

import Data.List (nub, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)

-- Data types

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

-- Functions

-- Insert a value into a tree.
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x <= y    = Node (insert x left) y right
  | otherwise = Node left y (insert x right)

-- Find a value in a tree.
find :: Ord a => a -> Tree a -> Maybe a
find x Leaf = Nothing
find x (Node left y right)
  | x == y    = Just y
  | x < y     = find x left
  | otherwise = find x right

-- Delete a value from a tree.
delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Node left y right)
  | x < y     = Node (delete x left) y right
  | x > y     = Node left y (delete x right)
  | otherwise = merge left right

-- Merge two trees into one.
merge :: Ord a => Tree a -> Tree a -> Tree a
merge Leaf right = right
merge left Leaf = left
merge (Node left1 y1 right1) (Node left2 y2 right2) =
  let y = min y1 y2
  in Node (merge left1 (Node Leaf y left2)) y (merge right1 right2)

-- Convert a list of values into a tree.
listToTree :: Ord a => [a] -> Tree a
listToTree xs = foldl insert Leaf xs

-- Convert a tree into a list of values.
treeToList :: Ord a => Tree a -> [a]
treeToList Leaf = []
treeToList (Node left y right) = treeToList left ++ [y] ++ treeToList right

-- Find the minimum value in a tree.
minTree :: Ord a => Tree a -> Maybe a
minTree Leaf = Nothing
minTree (Node left y right) = fromMaybe y (minTree left)

-- Find the maximum value in a tree.
maxTree :: Ord a => Tree a -> Maybe a
maxTree Leaf = Nothing
maxTree (Node left y right) = fromMaybe y (maxTree right)

-- Find the height of a tree.
height :: Tree a -> Int
height Leaf = 0
height (Node left _ right) = 1 + max (height left) (height right)

-- Check if a tree is balanced.
isBalanced :: Ord a => Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node left y right) =
  let hLeft = height left
      hRight = height right
  in abs (hLeft - hRight) <= 1 && isBalanced left && isBalanced right

-- Find the longest path in a tree.
longestPath :: Tree a -> Int
longestPath Leaf = 0
longestPath (Node left _ right) = 1 + max (longestPath left) (longestPath right)

-- Find the diameter of a tree.
diameter :: Tree a -> Int
diameter Leaf = 0
diameter (Node left _ right) = max (diameter left) (diameter right) + 1

-- Find the number of leaves in a tree.
numLeaves :: Tree a -> Int
numLeaves Leaf = 0
numLeaves (Node left _ right) = numLeaves left + numLeaves right + 1

-- Find the number of internal nodes in a tree.
numInternalNodes :: Tree a -> Int
numInternalNodes Leaf = 0
numInternalNodes (Node _ _ _) = 1 + numInternalNodes left + numInternalNodes right

-- Find the level order traversal of a tree.
levelOrder :: Tree a -> [[a]]
levelOrder Leaf = []
levelOrder tree = levelOrder' [tree] []
  where
    levelOrder' [] acc = acc
    levelOrder' (Node left y right : rest) acc =
      levelOrder' (left : right : rest) (acc ++ [[y]])

-- Find the preorder traversal of a tree.
preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node left y right) = y : preorder left ++ preorder right

-- Find the inorder traversal of a tree.
inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node left y right) = inorder left ++ [y] ++ inorder right

-- Find the postorder traversal of a tree.
postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node left y right) = postorder left ++ postorder right ++ [y]

-- Find the boundary traversal of a tree.
boundary :: Tree a -> [a]
boundary Leaf = []
boundary (Node left y right) = leftmost ++ [y] ++ rightmost
  where
    leftmost = map head (levelOrder left)
    rightmost = map last (levelOrder right)

-- Find the ancestors of a value in a tree.
ancestors :: Ord a => a -> Tree a -> [a]
ancestors x Leaf = []
ancestors x (Node left y right)
  | x == y    = []
  | x < y     = x : ancestors x left
  | otherwise = x : ancestors x right

-- Find the descendants of a value in a tree.
descendants :: Ord a => a -> Tree a -> [a]
descendants x Leaf = []
descendants x (Node left y right)
  | x == y    = treeToList (Node left y right)
  | x < y     = descendants x left ++ descendants x right
  | otherwise = descendants x right

-- Find the common ancestors of two values in a tree.
commonAncestors :: Ord a => a -> a -> Tree a -> [a]
commonAncestors x y Leaf = []
commonAncestors x y (Node left z right)
  | x == z || y == z    = [z]
  | x < z && y < z      = commonAncestors x y left
  | x > z && y > z      = commonAncestors x y right
  | otherwise           = commonAncestors x z right ++ commonAncestors y z left

-- Find the lowest common ancestor of two values in a tree.
lca :: Ord a => a -> a -> Tree a -> Maybe a
lca x y Leaf = Nothing
lca x y (Node left z right)
  | x == z || y == z    = Just z
  | x < z && y < z      = lca x y left
  | x > z && y > z      = lca x y right
  | otherwise           = Just z

-- Find the distance between two values in a tree.
distance :: Ord a => a -> a -> Tree a -> Maybe Int
distance x y Leaf = Nothing
distance x y (Node left z right)
  | x == z || y == z    = Just 0
  | x < z && y < z      = distance x y left
  | x > z && y > z      = distance x y right
  | otherwise           = Just $ 1 + fromMaybe 0 (distance x z left) + fromMaybe 0 (distance y z right)

-- Find the path between two values in a tree.
path :: Ord a => a -> a -> Tree a -> Maybe [a]
path x y Leaf = Nothing
path x y (Node left z right)
  | x == z || y == z    = Just [z]
  | x < z && y < z      = path x y left
  | x > z && y > z      = path x y right
  | otherwise           = Just $ z : fromMaybe [] (path x y left) ++ fromMaybe [] (path y z right)

-- Find the kth smallest value in a tree.
kthSmallest :: Ord a => Int -> Tree a -> Maybe a
kthSmallest _ Leaf = Nothing
kthSmallest k (Node left y right)
  | k <= size left    = kthSmallest k left
  | k == size left + 1 = Just y
  | otherwise           = kthSmallest (k - size left - 1) right
  where
    size Leaf = 0
    size (Node left _ right) = 1 + size left + size right

-- Find the kth largest value in a tree.
kthLargest :: Ord a => Int -> Tree a -> Maybe a
kthLargest _ Leaf = Nothing
kthLargest k (Node left y right)
  | k <= size right    = kthLargest k right
  | k == size right + 1 = Just y
  | otherwise           = kthLargest (k - size right - 1) left
  where
    size Leaf = 0
    size (Node left _ right) = 1 + size left + size right

-- Find the median of a tree.
median :: Ord a => Tree a -> Maybe a
median tree = kthSmallest (div (numLeaves tree) 2 + 1) tree

-- Find the mode of a tree.
mode :: Ord a => Tree a -> [a]
mode tree =
  let values = treeToList tree
      freq = foldl (\acc x ->