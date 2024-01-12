```haskell
import Control.Monad (join)
import Data.List (find, groupBy, sort)
import Data.Map (Map)
import qualified Data.Map as Map

-- Define a type to represent a node in a binary tree.
data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

-- Define a function to insert a value into a binary tree.
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x <= y    = Node (insert x left) y right
  | otherwise = Node left y (insert x right)

-- Define a function to find the minimum value in a binary tree.
minimum :: Ord a => Tree a -> Maybe a
minimum Leaf = Nothing
minimum (Node left y right) =
  case minimum left of
    Just x -> Just x
    Nothing -> Just y

-- Define a function to find the maximum value in a binary tree.
maximum :: Ord a => Tree a -> Maybe a
maximum Leaf = Nothing
maximum (Node left y right) =
  case maximum right of
    Just x -> Just x
    Nothing -> Just y

-- Define a function to search for a value in a binary tree.
search :: Ord a => a -> Tree a -> Bool
search x Leaf = False
search x (Node left y right)
  | x == y    = True
  | x <= y    = search x left
  | otherwise = search x right

-- Define a function to delete a value from a binary tree.
delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Node left y right)
  | x == y    = case (minimum right, maximum left) of
                 (Just x', Just y') -> Node left y' (delete x' right)
                 _                   -> right
  | x <= y    = Node (delete x left) y right
  | otherwise = Node left y (delete x right)

-- Define a function to convert a binary tree to a list.
toList :: Tree a -> [a]
toList Leaf = []
toList (Node left y right) = toList left ++ [y] ++ toList right

-- Define a function to convert a list to a binary tree.
fromList :: Ord a => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = insert x (fromList xs)

-- Define a function to find the height of a binary tree.
height :: Tree a -> Int
height Leaf = 0
height (Node left y right) = 1 + max (height left) (height right)

-- Define a function to find the number of nodes in a binary tree.
size :: Tree a -> Int
size Leaf = 0
size (Node left y right) = 1 + size left + size right

-- Define a function to find the number of leaves in a binary tree.
numLeaves :: Tree a -> Int
numLeaves Leaf = 1
numLeaves (Node left y right) = numLeaves left + numLeaves right

-- Define a function to find the number of internal nodes in a binary tree.
numInternalNodes :: Tree a -> Int
numInternalNodes Leaf = 0
numInternalNodes (Node left y right) = 1 + numInternalNodes left + numInternalNodes right

-- Define a function to find the level of a node in a binary tree.
level :: Ord a => a -> Tree a -> Maybe Int
level x Leaf = Nothing
level x (Node left y right)
  | x == y    = Just 0
  | x <= y    = join $ level x left
  | otherwise = join $ level x right

-- Define a function to find the path from the root to a node in a binary tree.
path :: Ord a => a -> Tree a -> Maybe [a]
path x Leaf = Nothing
path x (Node left y right)
  | x == y    = Just [y]
  | x <= y    = join $ path x left
  | otherwise = join $ path x right

-- Define a function to find the lowest common ancestor of two nodes in a binary tree.
lca :: Ord a => a -> a -> Tree a -> Maybe a
lca x y Leaf = Nothing
lca x y (Node left z right)
  | x <= z && y <= z    = lca x y left
  | x > z && y > z     = lca x y right
  | otherwise          = Just z

-- Define a function to find all the nodes in a binary tree that are at a given level.
nodesAtLevel :: Ord a => Int -> Tree a -> [a]
nodesAtLevel n Leaf = []
nodesAtLevel n (Node left y right)
  | n == 0    = [y]
  | n > 0     = nodesAtLevel (n-1) left ++ nodesAtLevel (n-1) right

-- Define a function to find all the nodes in a binary tree that are on a given path.
nodesOnPath :: Ord a => [a] -> Tree a -> [a]
nodesOnPath path Leaf = []
nodesOnPath path (Node left y right)
  | path == []         = [y]
  | head path == y     = y : nodesOnPath (tail path) left ++ nodesOnPath (tail path) right
  | head path < y      = nodesOnPath path left
  | otherwise         = nodesOnPath path right

-- Define a function to find all the nodes in a binary tree that are at a given distance from a given node.
nodesAtDistance :: Ord a => Int -> a -> Tree a -> [a]
nodesAtDistance n x Leaf = []
nodesAtDistance n x (Node left y right)
  | n == 0    = [y]
  | n > 0     = nodesAtDistance (n-1) x left ++ nodesAtDistance (n-1) x right ++ nodesAtDistance n x left ++ nodesAtDistance n x right

-- Define a function to find all the leaf nodes in a binary tree.
leafNodes :: Tree a -> [a]
leafNodes Leaf = []
leafNodes (Node left y right) = leafNodes left ++ [y] ++ leafNodes right

-- Define a function to find all the internal nodes in a binary tree.
internalNodes :: Tree a -> [a]
internalNodes Leaf = []
internalNodes (Node left y right) = [y] ++ internalNodes left ++ internalNodes right

-- Define a function to find all the nodes in a binary tree that are greater than a given value.
nodesGreaterThan :: Ord a => a -> Tree a -> [a]
nodesGreaterThan x Leaf = []
nodesGreaterThan x (Node left y right)
  | x < y      = nodesGreaterThan x left ++ [y] ++ nodesGreaterThan x right
  | otherwise = nodesGreaterThan x right

-- Define a function to find all the nodes in a binary tree that are less than a given value.
nodesLessThan :: Ord a => a -> Tree a -> [a]
nodesLessThan x Leaf = []
nodesLessThan x (Node left y right)
  | x > y      = nodesLessThan x left ++ [y] ++ nodesLessThan x right
  | otherwise = nodesLessThan x left

-- Define a function to find all the nodes in a binary tree that are between two given values.
nodesBetween :: Ord a => a -> a -> Tree a -> [a]
nodesBetween x y Leaf = []
nodesBetween x y (Node left z right)
  | x <= z && z <= y    = nodesBetween x y left ++ [z] ++ nodesBetween x y right
  | x > z              = nodesBetween x y right
  | otherwise          = nodesBetween x y left

-- Define a function to find all the paths from the root to a given node in a binary tree.
pathsToNode :: Ord a => a -> Tree a -> [[a]]
pathsToNode x Leaf = []
pathsToNode x (Node left y right)
  | x == y    = [[y]]
  | x <= y    = join $ pathsToNode x left
  | otherwise = join $ pathsToNode x right

-- Define a function to find all the paths from the root to a given level in a binary tree.
pathsToLevel :: Int -> Tree a -> [[a]]
pathsToLevel n Leaf = []
pathsToLevel n (Node left y right)
  | n == 0    = [[y]]
  | n > 0     = join $ pathsToLevel (n-1) left ++ join $ pathsToLevel (n-1) right

-- Define a function to find all the paths from the root to a given distance from a given node in a binary tree.
pathsToDistance :: Ord a => Int -> a -> Tree a -> [[a]]
pathsToDistance n x Leaf = []
pathsToDistance n x (Node left y right)
  | n == 0    = [[y]]
