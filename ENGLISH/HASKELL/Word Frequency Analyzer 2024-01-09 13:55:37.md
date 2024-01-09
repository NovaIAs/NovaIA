```haskell
-- Import necessary libraries
import Data.List (sort, group, nub)
import Data.Ord (comparing)
import Control.Monad (forM_, zipWithM_)
import Data.Ratio (Ratio, (%))
import Data.Char (toUpper)

-- Define the main function
main :: IO ()
main = do
  -- Read input from the user
  input <- getLine
  -- Split the input into a list of words
  words <- words input
  -- Convert all words to uppercase
  words' <- forM words $ \w -> return $ map toUpper w
  -- Remove duplicate words
  uniqueWords <- nub words'
  -- Sort the list of unique words
  sortedWords <- sort $ map toUpper words'
  -- Create a map from each word to the number of occurrences
  wordFreqMap <- foldM (\m w -> Map.adjust (+1) w m) Map.empty sortedWords
  -- Find the maximum number of occurrences
  maxFreq <- maximum $ Map.elems wordFreqMap
  -- Filter the map to include only words with the maximum frequency
  maxFreqWords <- filter ((==maxFreq) . Map.lookup) wordFreqMap sortedWords
  -- Print the results
  putStrLn "Input:"
  putStrLn $ unwords input
  putStrLn "Unique Words:"
  putStrLn $ unwords uniqueWords
  putStrLn "Sorted Words:"
  putStrLn $ unwords sortedWords
  putStrLn "Word Frequencies:"
  forM_ (Map.assocs wordFreqMap) $ \(w, f) -> putStrLn $ w ++ ": " ++ show f
  putStrLn "Maximum Frequency:"
  putStrLn $ show maxFreq
  putStrLn "Words with Maximum Frequency:"
  putStrLn $ unwords maxFreqWords

```

Explanation:

1. Import necessary libraries: This line imports the necessary Haskell libraries that will be used in the code.

2. Define the `main` function: This is the entry point of the program.

3. Read input from the user: The `getLine` function is used to read a line of text from the user and store it in the `input` variable.

4. Split the input into a list of words: The `words` function is used to split the input text into a list of words.

5. Convert all words to uppercase: The `map` function is used to convert each word in the list to uppercase using the `toUpper` function.

6. Remove duplicate words: The `nub` function is used to remove duplicate words from the list of words.

7. Sort the list of unique words: The `sort` function is used to sort the list of unique words in alphabetical order.

8. Create a map from each word to the number of occurrences: The `foldM` function is used to create a map from each word to the number of occurrences. The `Map.adjust` function is used to increment the count of each word in the map.

9. Find the maximum number of occurrences: The `maximum` function is used to find the maximum number of occurrences of any word in the map.

10. Filter the map to include only words with the maximum frequency: The `filter` function is used to filter the map to include only words with the maximum frequency.

11. Print the results: The `putStrLn` function is used to print the results to the console.