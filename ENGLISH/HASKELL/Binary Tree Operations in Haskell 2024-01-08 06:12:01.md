```haskell
-- Define a data type to represent a binary tree.
data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- Define a function to insert a value into a binary tree.
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x <= y = Node (insert x left) y right
  | otherwise = Node left y (insert x right)

-- Define a function to search for a value in a binary tree.
search :: Ord a => a -> Tree a -> Bool
search x Leaf = False
search x (Node left y right)
  | x == y = True
  | x < y = search x left
  | otherwise = search x right

-- Define a function to delete a value from a binary tree.
delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Node left y right)
  | x == y = merge left right
  | x < y = Node (delete x left) y right
  | otherwise = Node left y (delete x right)
  where
    merge :: Tree a -> Tree a -> Tree a
    merge Leaf Leaf = Leaf
    merge (Node left1 y1 right1) (Node left2 y2 right2) =
      let z = minimum (y2:right2)
      in Node (merge left1 (delete z (Node left2 y2 right2))) z (merge right1 (delete z (Node left2 y2 right2))))

-- Define a function to find the minimum value in a binary tree.
minimum :: Ord a => Tree a -> a
minimum (Node left y right) = minimum left

-- Define a function to find the maximum value in a binary tree.
maximum :: Ord a => Tree a -> a
maximum (Node left y right) = maximum right

-- Define a function to find the height of a binary tree.
height :: Tree a -> Int
height Leaf = 0
height (Node left y right) = 1 + max (height left) (height right)

-- Define a function to print a binary tree.
printTree :: Tree a -> IO ()
printTree Leaf = putStrLn "Leaf"
printTree (Node left y right) = do
  putStrLn ("Node " ++ show y)
  printTree left
  printTree right

-- Define a function to create a binary tree from a list of values.
createTree :: Ord a => [a] -> Tree a
createTree [] = Leaf
createTree (x:xs) = insert x (createTree xs)

-- Define a function to test the binary tree functions.
main :: IO ()
main = do
  let tree = createTree [1, 2, 3, 4, 5, 6, 7, 8, 9]
  printTree tree
  putStrLn ("Minimum: " ++ show (minimum tree))
  putStrLn ("Maximum: " ++ show (maximum tree))
  putStrLn ("Height: " ++ show (height tree))
  putStrLn ("Search for 5: " ++ show (search 5 tree))
  putStrLn ("Delete 5: ")
  printTree (delete 5 tree)
```

This code defines a data type to represent a binary tree, and a number of functions to manipulate binary trees. The `insert` function inserts a value into a binary tree, the `search` function searches for a value in a binary tree, the `delete` function deletes a value from a binary tree, the `minimum` function finds the minimum value in a binary tree, the `maximum` function finds the maximum value in a binary tree, the `height` function finds the height of a binary tree, the `printTree` function prints a binary tree, the `createTree` function creates a binary tree from a list of values, and the `main` function tests the binary tree functions.