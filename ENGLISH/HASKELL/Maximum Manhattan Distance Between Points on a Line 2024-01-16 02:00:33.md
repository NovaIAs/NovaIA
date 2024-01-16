```haskell
import Data.List (nub, sort)
import Data.Ord (comparing)
import Data.String (lines)
import System.Environment (getArgs)

type Coord = (Int, Int)
type Line = (Coord, Coord)

main :: IO ()
main = do
  args <- getArgs
  lines <- readFile (head args)
  let lines' = map (map read . words) $ lines
  print $ maximum $ map (maximum . map manhattan) lines'
  print $ maximum $ map (maximum . map (sum . map manhattan) . groupByFirst) lines'

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

groupByFirst :: Eq a => [Line] -> [[Line]]
groupByFirst = groupBy (\(a1, _1) (a2, _2) -> a1 == a2)

```
This code reads a file containing lines in the form "x1,y1 -> x2,y2" and computes the maximum Manhattan distance between any two points on any line, as well as the maximum total Manhattan distance between any two points on any line.

The code uses the `Data.List` module to sort and remove duplicates from the list of lines, and the `Data.Ord` module to compare the lines by their first coordinate.

The `manhattan` function computes the Manhattan distance between two points, and the `groupByFirst` function groups the lines by their first coordinate.

The `maximum` function is used to find the maximum value in a list, and the `map` function is used to apply a function to each element in a list.

The `print` function is used to output the results to the console.