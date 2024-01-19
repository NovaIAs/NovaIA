```haskell
import Data.List (unfoldr)
import Data.Ord (comparing)

-- Data types

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

-- Leaf and Branch constructors

leaf :: a -> Tree a
leaf x = Leaf x

branch :: Tree a -> Tree a -> Tree a
branch l r = Branch l r

-- Tree traversal using unfoldr

unfoldTree :: (a -> Maybe (b, a)) -> a -> [b]
unfoldTree f s = unfoldr (traverse s f)

traverse :: a -> (a -> Maybe (b, a)) -> Maybe (b, a)
traverse s f = case f s of
  Nothing -> Nothing
  Just (x, s') -> Just (x, s')

-- Tree traversal using recursion

inorder :: Tree a -> [a]
inorder (Leaf x) = [x]
inorder (Branch l r) = inorder l ++ inorder r

preorder :: Tree a -> [a]
preorder (Leaf x) = [x]
preorder (Branch l r) = [x] ++ preorder l ++ preorder r

postorder :: Tree a -> [a]
postorder (Leaf x) = [x]
postorder (Branch l r) = postorder l ++ postorder r ++ [x]

-- Tree searching using recursion

search :: Ord a => Tree a -> a -> Maybe Tree a
search (Leaf x) y = if x == y then Just (Leaf x) else Nothing
search (Branch l r) y
  | y < x = search l y
  | y > x = search r y
  | otherwise = Just (Branch l r)

-- Tree insertion using recursion

insert :: Ord a => Tree a -> a -> Tree a
insert (Leaf x) y = if x == y then Leaf x else Branch (Leaf x) (Leaf y)
insert (Branch l r) y
  | y < x = Branch (insert l y) r
  | y > x = Branch l (insert r y)
  | otherwise = Branch l r

-- Tree deletion using recursion

delete :: Ord a => Tree a -> a -> Tree a
delete (Leaf x) y = if x == y then Leaf x else error "not found"
delete (Branch l r) y
  | y < x = Branch (delete l y) r
  | y > x = Branch l (delete r y)
  | otherwise = merge l r

merge :: Ord a => Tree a -> Tree a -> Tree a
merge (Leaf x) (Leaf y) = Leaf (min x y)
merge (Leaf x) (Branch l r) = Branch (Leaf x) (merge (Leaf x) r)
merge (Branch l r) (Leaf y) = Branch (merge l (Leaf y)) (Leaf y)
merge (Branch l1 r1) (Branch l2 r2) =
  let x = min (x1, x2)
      y = max (y1, y2)
      (l', r') = splitAt (x - x1) l1
      (l'', r'') = splitAt (y - y2) r2
  in Branch
       (merge l' (merge l'' (Leaf x)))
       (merge r' (merge r'' (Leaf y)))
    where
      (x1, l1, r1) = go l1
      (y1, l2, r2) = go r2

go :: Tree a -> (a, Tree a, Tree a)
go (Leaf x) = (x, Leaf x, Leaf x)
go (Branch l r) = (x, l', r')
  where
    (x, l', r') = go l

-- Example usage

tree :: Tree Int
tree = leaf 100 `branch` branch (leaf 50) (branch (leaf 25) (leaf 75))

main :: IO ()
main = do
  print $ inorder tree
  print $ preorder tree
  print $ postorder tree
  print $ search tree 50
  print $ insert tree 125
  print $ delete tree 75
```

This code implements a binary search tree in Haskell. It includes functions for creating trees, traversing them, searching for elements in them, inserting elements into them, and deleting elements from them. It also includes an example of a binary search tree and how to use the functions to work with it.

Here is a breakdown of the code:

* **Data types:** The code defines a data type called `Tree a` to represent binary search trees. A tree can either be a `Leaf` containing a single element or a `Branch` containing two subtrees, a left subtree and a right subtree.

* **Leaf and Branch constructors:** The code defines the `leaf` and `branch` functions to create leaf and branch nodes, respectively.

* **Tree traversal using unfoldr:** The code defines the `unfoldTree` function to traverse a tree using the `unfoldr` function from the `Data.List` module. This function takes a function `f` that takes a tree node and returns a tuple containing the node's value and a new tree node, and a starting tree node `s`. It returns a list of the values of the nodes in the tree in the order determined by the traversal function `f`.

* **Tree traversal using recursion:** The code defines the `inorder`, `preorder`, and `postorder` functions to traverse a tree using recursion. These functions take a tree as an argument and return a list of the values of the nodes in the tree in inorder, preorder, and postorder, respectively.

* **Tree searching using recursion:** The code defines the `search` function to search for an element in a tree using recursion. This function takes a tree and an element as arguments and returns a `Maybe` value containing the tree node containing the element if it is found, or `Nothing` if it is not found.

* **Tree insertion using recursion:** The code defines the `insert` function to insert an element into a tree using recursion. This function takes a tree and an element as arguments and returns a new tree with the element inserted into it.

* **Tree deletion using recursion:** The code defines the `delete` function to delete an element from a tree using recursion. This function takes a tree and an element as arguments and returns a new tree with the element deleted from it.

* **Merge function:** The code defines the `merge` function to merge two trees into one tree. This function takes two trees as arguments and returns a new tree that is the union of the two trees.

* **Example usage:** The code defines an example binary search tree and demonstrates how to use the functions to work with it.

Overall, this code provides a comprehensive implementation of a binary search tree in Haskell, including functions for creating trees, traversing them, searching for elements in them, inserting elements into them, and deleting elements from them.