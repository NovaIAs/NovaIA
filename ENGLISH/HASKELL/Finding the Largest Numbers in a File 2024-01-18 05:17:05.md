```haskell
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

-- Parse command line arguments
args <- getArgs
let (input, n) = case args of
    [n, input] -> (input, read n :: Int)
    _ -> error "Usage: program n input"

-- Read input file
inputContents <- readFile input
let lines = words inputContents

-- Convert lines to integers
let numbers = map read lines :: [Int]

-- Find the n largest numbers
let largest = take n $ sort (nub numbers)

-- Print the largest numbers
putStrLn $ unwords $ map show largest
```

This Haskell code finds the n largest numbers in a given input file. It uses the following functions:

* `Data.List.nub`: Removes duplicate elements from a list.
* `Data.List.sort`: Sorts a list in ascending order.
* `Data.Maybe.fromJust`: Extracts the value from a `Maybe` value, or throws an error if the value is `Nothing`.
* `System.Environment.getArgs`: Gets the command line arguments.

The code first parses the command line arguments, which should be the name of the input file and the number of largest numbers to find. It then reads the contents of the input file and splits it into a list of words. The words are then converted to integers and the n largest numbers are found using the `take` and `sort` functions. Finally, the largest numbers are printed to the standard output.