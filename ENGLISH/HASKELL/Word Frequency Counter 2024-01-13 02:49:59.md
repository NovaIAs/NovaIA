```haskell
import Data.List (sort)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

-- Define the main function
main :: IO ()
main = do
  -- Get the command-line arguments
  args <- getArgs

  -- Check if the number of arguments is correct
  if length args /= 2 then
    error "Usage: program input-file output-file"
  else
    -- Read the input file
    input <- readFile (args !! 0)

  -- Parse the input file
  let lines = words input

  -- Count the number of occurrences of each word
  let wordCounts = foldl' (\counts word -> insertWith (+) 1 word counts) mempty lines

  -- Sort the word counts in descending order
  let sortedCounts = sort (reverse $ toList wordCounts)

  -- Write the sorted word counts to the output file
  writeFile (args !! 1) $ unlines $ map (\(word, count) -> word ++ " " ++ show count) sortedCounts

-- Define a function to insert a value into a map, using a given function to combine
-- values if the key already exists
insertWith :: (a -> a -> a) -> a -> k -> Map k a -> Map k a
insertWith f value key map =
  let existingValue = fromMaybe 0 (lookup key map)
  in insert key (f existingValue value) map

-- Define a function to convert a map to a list of key-value pairs
toList :: Map k a -> [(k, a)]
toList map = [(key, value) | (key, value) <- associations map]
```

This code is a program that takes two command-line arguments: the name of an input file and the name of an output file. The program reads the input file, which is a text file containing a list of words, and counts the number of occurrences of each word. The program then sorts the word counts in descending order and writes the sorted list to the output file.

The code uses the `Data.List` and `Data.Maybe` modules from the Haskell standard library, as well as the `System.Environment` module to get the command-line arguments.

The `main` function is the entry point of the program. It first gets the command-line arguments using the `getArgs` function. Then, it checks if the number of arguments is correct. If it is not, the program prints an error message and exits.

If the number of arguments is correct, the program reads the input file using the `readFile` function. The `words` function is then used to split the input file into a list of words.

The `foldl'` function is used to count the number of occurrences of each word. The `insertWith` function is used to insert each word into a map, using the `(+)` function to combine the counts if the word already exists in the map.

The `sort` function is used to sort the word counts in descending order. The `toList` function is then used to convert the map of word counts to a list of key-value pairs.

Finally, the `writeFile` function is used to write the sorted list of word counts to the output file.