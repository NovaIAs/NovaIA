```haskell
import Data.List (sortBy)

-- A data type to represent a node in a binary tree.
data Node a = Leaf a | Branch (Node a) a (Node a)
  deriving (Show, Eq)

-- A function to create a binary tree from a list of values.
createTree :: [a] -> Node a
createTree [] = Leaf (error "Cannot create a tree from an empty list.")
createTree [x] = Leaf x
createTree (x:xs) = Branch (createTree xs) x (createTree (dropWhile (< x) xs))

-- A function to find the maximum value in a binary tree.
maximumTree :: Ord a => Node a -> a
maximumTree (Leaf x) = x
maximumTree (Branch l y r) = maximum [maximumTree l, y, maximumTree r]

-- A function to find the minimum value in a binary tree.
minimumTree :: Ord a => Node a -> a
minimumTree (Leaf x) = x
minimumTree (Branch l y r) = minimum [minimumTree l, y, minimumTree r]

-- A function to find the height of a binary tree.
heightTree :: Node a -> Int
heightTree (Leaf _) = 0
heightTree (Branch l _ r) = maximum [heightTree l, heightTree r] + 1

-- A function to find the number of nodes in a binary tree.
sizeTree :: Node a -> Int
sizeTree (Leaf _) = 1
sizeTree (Branch l _ r) = sizeTree l + sizeTree r + 1

-- A function to find the longest path from the root to a leaf in a binary tree.
longestPath :: Node a -> Int
longestPath (Leaf _) = 0
longestPath (Branch l _ r) = maximum [longestPath l, longestPath r] + 1

-- A function to find the level-order traversal of a binary tree.
levelOrder :: Node a -> [[a]]
levelOrder (Leaf x) = [[x]]
levelOrder (Branch l y r) = levels ++ [[y]]
  where levels = concatMap levelOrder [l, r]

-- A function to find the inorder traversal of a binary tree.
inorder :: Node a -> [a]
inorder (Leaf x) = [x]
inorder (Branch l y r) = inorder l ++ [y] ++ inorder r

-- A function to find the preorder traversal of a binary tree.
preorder :: Node a -> [a]
preorder (Leaf x) = [x]
preorder (Branch l y r) = [y] ++ preorder l ++ preorder r

-- A function to find the postorder traversal of a binary tree.
postorder :: Node a -> [a]
postorder (Leaf x) = [x]
postorder (Branch l y r) = postorder l ++ postorder r ++ [y]

-- A function to find all the paths from the root to a leaf in a binary tree.
paths :: Node a -> [[a]]
paths (Leaf x) = [[x]]
paths (Branch l y r) = paths l ++ map (y:) (paths r)

-- A function to find the number of leaves in a binary tree.
numLeaves :: Node a -> Int
numLeaves (Leaf _) = 1
numLeaves (Branch l _ r) = numLeaves l + numLeaves r

-- A function to find the number of internal nodes in a binary tree.
numInternalNodes :: Node a -> Int
numInternalNodes (Leaf _) = 0
numInternalNodes (Branch l _ r) = 1 + numInternalNodes l + numInternalNodes r

-- A function to find the sum of all the values in a binary tree.
sumTree :: Num a => Node a -> a
sumTree (Leaf x) = x
sumTree (Branch l y r) = sumTree l + y + sumTree r

-- A function to find the average value in a binary tree.
averageTree :: Fractional a => Node a -> a
averageTree (Leaf x) = x
averageTree (Branch l y r) = (sumTree (Branch l y r)) / (sizeTree (Branch l y r))

-- A function to find the most frequent value in a binary tree.
mostFrequent :: Ord a => Node a -> a
mostFrequent (Leaf x) = x
mostFrequent (Branch l y r) =
  let frequencies = map (\x -> (x, count x (Branch l y r))) (nub (inorder (Branch l y r)))
   in maximumBy (\(a, b) (c, d) -> compare d b) frequencies

-- A function to count the number of occurrences of a value in a binary tree.
count :: Eq a => a -> Node a -> Int
count x (Leaf y) = if x == y then 1 else 0
count x (Branch l y r) = count x l + count x y + count x r

-- A function to find the lowest common ancestor of two nodes in a binary tree.
lca :: Ord a => a -> a -> Node a -> Node a
lca x y (Branch l z r)
  | x == y = (Branch l z r)
  | x < y = lca x y l
  | otherwise = lca x y r
lca x y (Leaf z) = Leaf z

-- A function to find the distance between two nodes in a binary tree.
distance :: Ord a => a -> a -> Node a -> Int
distance x y (Branch l z r)
  | x == y = 0
  | x < y = 1 + distance x y l
  | otherwise = 1 + distance x y r
distance x y (Leaf z) = error "The nodes are not in the tree."

-- A function to find the height of a node in a binary tree.
heightNode :: Ord a => a -> Node a -> Int
heightNode x (Leaf y)
  | x == y = 0
  | otherwise = error "The node is not in the tree."
heightNode x (Branch l z r)
  | x == z = 0
  | x < z = 1 + heightNode x l
  | otherwise = 1 + heightNode x r

-- A function to find the level of a node in a binary tree.
levelNode :: Ord a => a -> Node a -> Int
levelNode x (Branch l z r)
  | x == z = 0
  | x < z = 1 + levelNode x l
  | otherwise = 1 + levelNode x r
levelNode x (Leaf y)
  | x == y = 0
  | otherwise = error "The node is not in the tree."

-- A function to find the subtree of a binary tree rooted at a given node.
subtree :: Ord a => a -> Node a -> Node a
subtree x (Branch l z r)
  | x == z = (Branch l z r)
  | x < z = subtree x l
  | otherwise = subtree x r
subtree x (Leaf y)
  | x == y = (Leaf y)
  | otherwise = error "The node is not in the tree."

-- A function to find the mirror image of a binary tree.
mirror :: Node a -> Node a
mirror (Leaf x) = (Leaf x)
mirror (Branch l y r) = (Branch (mirror r) y (mirror l))

-- A function to find the diameter of a binary tree.
diameter :: Node a -> Int
diameter (Branch l x r) = maximum [heightTree l + heightTree r + 1, diameter l, diameter r]
diameter (Leaf _) = 0

-- A function to find the boundary of a binary tree.
boundary :: Node a -> [a]
boundary (Leaf x) = [x]
boundary (Branch l y r) = concat [leftBoundary l, leafBoundary l, leafBoundary r, rightBoundary r]
  where leftBoundary (Leaf z) = [z]
        leftBoundary (Branch l z r) = z : leftBoundary l
        rightBoundary (Leaf z) = [z]
        rightBoundary (Branch l z r) = rightBoundary r ++ [z]
        leafBoundary (Leaf x) = [x]
        leafBoundary (Branch l y r) = leafBoundary l ++ leafBoundary r

-- A function to find the perimeter of a binary tree.
perimeter :: Node a -> Int
perimeter (Leaf _) = 0
perimeter (Branch l y r) = (perimeter l) + (perimeter r) + (heightTree l) + (heightTree r) + 1

-- A function to find the convex hull of a binary tree.
convexHull :: Node a -> [