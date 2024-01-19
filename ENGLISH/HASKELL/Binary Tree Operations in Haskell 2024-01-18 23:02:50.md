```haskell
import Control.Monad
import Data.List
import Data.Function
import Data.Maybe

-- Define the type of a binary tree
data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- Define the function to insert a value into a binary tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l y r)
  | x < y = Node (insert x l) y r
  | otherwise = Node l y (insert x r)

-- Define the function to find a value in a binary tree
find :: Ord a => a -> Tree a -> Maybe a
find x Leaf = Nothing
find x (Node l y r)
  | x == y = Just y
  | x < y = find x l
  | otherwise = find x r

-- Define the function to delete a value from a binary tree
delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Node l y r)
  | x == y = merge l r
  | x < y = Node (delete x l) y r
  | otherwise = Node l y (delete x r)
  where
    merge Leaf r = r
    merge l Leaf = l
    merge (Node l1 y1 r1) (Node l2 y2 r2) = Node (merge l1 l2) y1 (merge r1 r2)

-- Define the function to find the minimum value in a binary tree
minimum :: Ord a => Tree a -> a
minimum (Node Leaf x Leaf) = x
minimum (Node l y r) = minimum l

-- Define the function to find the maximum value in a binary tree
maximum :: Ord a => Tree a -> a
maximum (Node Leaf x Leaf) = x
maximum (Node l y r) = maximum r

-- Define the function to find the height of a binary tree
height :: Tree a -> Int
height Leaf = 0
height (Node l y r) = 1 + max (height l) (height r)

-- Define the function to find the number of nodes in a binary tree
size :: Tree a -> Int
size Leaf = 0
size (Node l y r) = 1 + size l + size r

-- Define the function to check if a binary tree is balanced
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node l y r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r

-- Define the function to construct a binary tree from a list of values
construct :: Ord a => [a] -> Tree a
construct [] = Leaf
construct (x:xs) = insert x (construct xs)

-- Define the function to print a binary tree
printTree :: Show a => Tree a -> String
printTree Leaf = ""
printTree (Node l y r) = printTree l ++ show y ++ " " ++ printTree r

-- Define the function to test the binary tree functions
main :: IO ()
main = do
  let t = construct [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  print $ find 5 t
  print $ delete 5 t
  print $ minimum t
  print $ maximum t
  print $ height t
  print $ size t
  print $ isBalanced t
  print $ printTree t
```

This code defines a binary tree data structure and a variety of functions to manipulate and query binary trees. The code includes functions to insert, find, delete, find the minimum and maximum values, find the height and size of a tree, check if a tree is balanced, construct a tree from a list of values, and print a tree.

To use this code, you can define a binary tree using the `construct` function, and then use the other functions to manipulate and query the tree. For example, to insert the value 5 into a tree, you would use the following code:

```haskell
let t = construct [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let t' = insert 5 t
```

You can then use the `find` function to find the value 5 in the tree:

```haskell
let result = find 5 t'
```

The `result` variable will be `Just 5`, indicating that the value 5 was found in the tree.

You can also use the `delete` function to delete the value 5 from the tree:

```haskell
let t'' = delete 5 t'
```

The `t''` variable will now be a tree that does not contain the value 5.

The other functions in this code can be used to perform a variety of other operations on binary trees. For example, you can use the `minimum` and `maximum` functions to find the minimum and maximum values in a tree, or you can use the `height` and `size` functions to find the height and size of a tree.