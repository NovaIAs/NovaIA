```haskell
import Data.List (nub, sort)
import Data.Map (Map, fromList, toList)
import Control.Monad (forM_, join)
import System.IO (hGetContents, openFile)

type Graph = Map String [String]

readGraph :: String -> IO Graph
readGraph filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  let lines = map words $ lines contents
  return $ fromList $ map (\(n, ns) -> (n, sort ns)) $ nub $ concatMap (\(u, v) -> [(u, v), (v, u)]) lines

findCliques :: Graph -> Int -> [String] -> [[String]]
findCliques graph k vs =
  let clique @ (vs, cs) = foldl' (\(vs, cs) v -> if v `elem` cs then (vs, cs) else (v : vs, v : cs)) (vs, []) vs
  in if all (`elem` vs) $ nub $ concatMap (graph !) vs then clique : findCliques graph k cs else []

findMaximalCliques :: Graph -> Int -> [[String]]
findMaximalCliques graph k =
  let cliques = findCliques graph k []
  in sort $ nub $ map (sort . nub) cliques

main :: IO ()
main = do
  graph <- readGraph "graph.txt"
  let cliques = findMaximalCliques graph 3
  mapM_ print cliques
```

This code finds all maximal cliques in a graph. A clique is a set of nodes in a graph such that there is an edge between every pair of nodes in the set. A maximal clique is a clique that is not a subset of any other clique.

The code first reads the graph from a file. The file format is a list of lines, each line containing two nodes connected by an edge.

The function `findCliques` finds all cliques of size `k` in the graph. It takes the graph, the size of the cliques to find, and a list of nodes that have already been visited. It returns a list of cliques, each represented as a pair of lists: the list of nodes in the clique and the list of nodes that are adjacent to all nodes in the clique.

The function `findMaximalCliques` finds all maximal cliques in the graph. It takes the graph and the size of the cliques to find. It returns a list of maximal cliques, each represented as a list of nodes.

The main function reads the graph from the file "graph.txt", finds all maximal cliques of size 3, and prints them to the console.