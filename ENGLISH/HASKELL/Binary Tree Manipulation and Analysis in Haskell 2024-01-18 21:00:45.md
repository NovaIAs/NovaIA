```haskell
-- Define a custom data type to represent a binary tree.
data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show, Eq, Ord, Functor)

-- Function to insert a value into a binary tree.
insert :: Ord a => a -> Tree a -> Tree a
insert x (Leaf y) = Node (Leaf y) x (Leaf y)
insert x (Node left y right)
  | x <= y    = Node (insert x left) y right
  | otherwise = Node left y (insert x right)

-- Function to delete a value from a binary tree.
delete :: Ord a => a -> Tree a -> Tree a
delete x (Leaf y)
  | x == y    = Leaf y
  | otherwise = Leaf y
delete x (Node left y right)
  | x == y    = merge left right
  | x < y     = Node (delete x left) y right
  | otherwise = Node left y (delete x right)
  where
    merge (Leaf l) (Leaf r) = Leaf l
    merge (Node l1 y1 r1) (Node l2 y2 r2) = Node (merge l1 l2) y1 (merge r1 r2)

-- Function to find the minimum value in a binary tree.
minimum :: Ord a => Tree a -> a
minimum (Leaf x) = x
minimum (Node left y right) = minimum left

-- Function to find the maximum value in a binary tree.
maximum :: Ord a => Tree a -> a
maximum (Leaf x) = x
maximum (Node left y right) = maximum right

-- Function to find the height of a binary tree.
height :: Tree a -> Int
height (Leaf _) = 0
height (Node left _ right) = 1 + max (height left) (height right)

-- Function to check if a binary tree is balanced.
isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node left y right) =
  abs (height left - height right) <= 1 &&
  isBalanced left &&
  isBalanced right

-- Function to print a binary tree in a visually appealing way.
printTree :: Show a => Tree a -> String
printTree (Leaf x) = "[" ++ show x ++ "]\n"
printTree (Node left y right) =
  "(" ++ printTree left ++ show y ++ printTree right ++ ")\n"

-- Example usage of the binary tree functions.
let tree = insert 10 (insert 5 (insert 15 (Leaf 20)))
in printTree tree
```

Explanation:

In this code, we define a custom data type called `Tree a` to represent a binary tree, where `a` is the type of the values stored in the tree. We also define several functions to manipulate and analyze binary trees:

1. `insert`: This function inserts a new value into a binary tree while maintaining the binary search tree property (i.e., the values in the left subtree are smaller than the value in the root node, and the values in the right subtree are greater than the value in the root node).

2. `delete`: This function deletes a value from a binary tree while maintaining the binary search tree property.

3. `minimum`: This function finds the minimum value in a binary tree.

4. `maximum`: This function finds the maximum value in a binary tree.

5. `height`: This function calculates the height of a binary tree, which is the maximum number of edges from the root node to any leaf node.

6. `isBalanced`: This function checks if a binary tree is balanced, meaning that the difference in heights between the left and right subtrees is at most 1.

7. `printTree`: This function prints a binary tree in a visually appealing way, using parentheses to represent the structure of the tree.

Finally, we provide an example usage of these functions by creating a binary tree and printing it using the `printTree` function.