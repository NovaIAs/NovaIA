```haskell
import Data.List (sortBy, tails)
import qualified Data.Map as M

-- Define a type synonym for a word and its frequency
type WordFreq = (String, Int)

-- Define a function to count the frequency of each word in a list of words
wordFreqs :: [String] -> [WordFreq]
wordFreqs = sortBy (flip compare) . map (\xs -> (head xs, length xs)) . tails

-- Define a function to build a map from words to their frequencies
wordFreqMap :: [String] -> M.Map String Int
wordFreqMap = M.fromList . wordFreqs

-- Define a function to print the top n most frequent words in a map
printTopN :: Int -> M.Map String Int -> IO ()
printTopN n = mapM_ print . take n . M.elems . M.sortBy (flip compare)

-- Define a function to main
main :: IO ()
main = do
  -- Read the input text file
  text <- readFile "input.txt"

  -- Convert the text to a list of words
  words <- words text

  -- Build a map of words to their frequencies
  freqMap <- wordFreqMap words

  -- Print the top 10 most frequent words
  printTopN 10 freqMap
```

**Explanation:**

1. We start by importing the necessary libraries.

2. We define a type synonym for a word and its frequency. This allows us to use a tuple to represent a word and its frequency in a more concise way.

3. We define a function called `wordFreqs` that takes a list of words as input and returns a list of tuples, where each tuple contains a word and its frequency. The frequency is calculated by counting the number of times the word appears in the list.

4. We define a function called `wordFreqMap` that takes a list of words as input and returns a map from words to their frequencies. The map is built by calling the `wordFreqs` function to get a list of word frequencies, and then converting the list to a map using the `M.fromList` function.

5. We define a function called `printTopN` that takes an integer `n` and a map from words to their frequencies as input, and prints the top `n` most frequent words in the map. The function first sorts the map by the values of the word frequencies in descending order, and then takes the first `n` elements of the sorted map. Finally, it prints each of the words in the list.

6. In the `main` function, we read the input text file, convert the text to a list of words, build a map of words to their frequencies, and then print the top 10 most frequent words in the map.