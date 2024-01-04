```haskell
-- Import the necessary modules.
import Data.List (sortBy, groupBy, nub)
import Data.Char (toUpper)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust)

-- Define the type of a word.
type Word = String

-- Define the type of a word frequency map.
type WordFrequencyMap = Map Word Int

-- Function to count the frequency of each word in a list of words.
wordFrequencyMap :: [Word] -> WordFrequencyMap
wordFrequencyMap words =
  fromList
    (map (\(w, n) -> (w, n))
      (groupBy ((==) `on` toUpper)
        (sortBy toUpper words)))

-- Function to find the most frequent word in a word frequency map.
mostFrequentWord :: WordFrequencyMap -> Word
mostFrequentWord wfm =
  fromJust
    (lookup (maximum (map snd (toList wfm))) wfm)

-- Function to find the least frequent word in a word frequency map.
leastFrequentWord :: WordFrequencyMap -> Word
leastFrequentWord wfm =
  fromJust
    (lookup (minimum (map snd (toList wfm))) wfm)

-- Function to find the most frequent words in a word frequency map.
mostFrequentWords :: Int -> WordFrequencyMap -> [Word]
mostFrequentWords n wfm =
  map fst
    (take n
      (sortBy (\(_, n1) (_, n2) -> compare n2 n1)
        (toList wfm)))

-- Function to find the least frequent words in a word frequency map.
leastFrequentWords :: Int -> WordFrequencyMap -> [Word]
leastFrequentWords n wfm =
  map fst
    (take n
      (sortBy (\(_, n1) (_, n2) -> compare n1 n2)
        (toList wfm)))

-- Function to find the unique words in a list of words.
uniqueWords :: [Word] -> [Word]
uniqueWords words = nub words

-- Function to find the total number of words in a list of words.
totalWords :: [Word] -> Int
totalWords = length

-- Function to find the average word length in a list of words.
averageWordLength :: [Word] -> Double
averageWordLength words =
  sum (map length words) / fromIntegral (totalWords words)

-- Function to find the longest word in a list of words.
longestWord :: [Word] -> Word
longestWord words =
  maximum words

-- Function to find the shortest word in a list of words.
shortestWord :: [Word] -> Word
shortestWord words =
  minimum words

-- Function to find the most common letter in a list of words.
mostCommonLetter :: [Word] -> Char
mostCommonLetter words =
  fromJust
    (lookup (maximum (map snd (toList (letterFrequencyMap words)))) (letterFrequencyMap words))

-- Function to count the frequency of each letter in a list of words.
letterFrequencyMap :: [Word] -> Map Char Int
letterFrequencyMap words =
  fromList
    (map (\(c, n) -> (c, n))
      (groupBy ((==) `on` toUpper)
        (sortBy toUpper (concat words))))

-- Function to find the least common letter in a list of words.
leastCommonLetter :: [Word] -> Char
leastCommonLetter words =
  fromJust
    (lookup (minimum (map snd (toList (letterFrequencyMap words)))) (letterFrequencyMap words))

-- Function to find the most frequent bigram in a list of words.
mostFrequentBigram :: [Word] -> (Word, Word)
mostFrequentBigram words =
  fromJust
    (lookup (maximum (map snd (toList (bigramFrequencyMap words)))) (bigramFrequencyMap words))

-- Function to count the frequency of each bigram in a list of words.
bigramFrequencyMap :: [Word] -> Map (Word, Word) Int
bigramFrequencyMap words =
  fromList
    (map (\((w1, w2), n) -> ((w1, w2), n))
      (groupBy ((==) `on` toUpper)
        (sortBy (\(w1, w2) (w3, w4) -> compare (w1, w2) (w3, w4))
          (zip words (tail words)))))

-- Function to find the least frequent bigram in a list of words.
leastFrequentBigram :: [Word] -> (Word, Word)
leastFrequentBigram words =
  fromJust
    (lookup (minimum (map snd (toList (bigramFrequencyMap words)))) (bigramFrequencyMap words))

-- Function to find the most frequent trigram in a list of words.
mostFrequentTrigram :: [Word] -> (Word, Word, Word)
mostFrequentTrigram words =
  fromJust
    (lookup (maximum (map snd (toList (trigramFrequencyMap words)))) (trigramFrequencyMap words))

-- Function to count the frequency of each trigram in a list of words.
trigramFrequencyMap :: [Word] -> Map (Word, Word, Word) Int
trigramFrequencyMap words =
  fromList
    (map (\((w1, w2, w3), n) -> ((w1, w2, w3), n))
      (groupBy ((==) `on` toUpper)
        (sortBy (\(w1, w2, w3) (w4, w5, w6) -> compare (w1, w2, w3) (w4, w5, w6))
          (zip words (tail words) (tail (tail words))))))

-- Function to find the least frequent trigram in a list of words.
leastFrequentTrigram :: [Word] -> (Word, Word, Word)
leastFrequentTrigram words =
  fromJust
    (lookup (minimum (map snd (toList (trigramFrequencyMap words)))) (trigramFrequencyMap words))

-- Function to find the most frequent 4-gram in a list of words.
mostFrequent4Gram :: [Word] -> (Word, Word, Word, Word)
mostFrequent4Gram words =
  fromJust
    (lookup (maximum (map snd (toList (4gramFrequencyMap words)))) (4gramFrequencyMap words))

-- Function to count the frequency of each 4-gram in a list of words.
4gramFrequencyMap :: [Word] -> Map (Word, Word, Word, Word) Int
4gramFrequencyMap words =
  fromList
    (map (\((w1, w2, w3, w4), n) -> ((w1, w2, w3, w4), n))
      (groupBy ((==) `on` toUpper)
        (sortBy (\(w1, w2, w3, w4) (w5, w6, w7, w8) -> compare (w1, w2, w3, w4) (w5, w6, w7, w8))
          (zip words (tail words) (tail (tail words)) (tail (tail (tail words)))))))

-- Function to find the least frequent 4-gram in a list of words.
leastFrequent4Gram :: [Word] -> (Word, Word, Word, Word)
leastFrequent4Gram words =
  fromJust
    (lookup (minimum (map snd (toList (4gramFrequencyMap words)))) (4gramFrequencyMap words))

-- Function to find the most frequent 5-gram in a list of words.
mostFrequent5Gram :: [Word] -> (Word, Word, Word, Word, Word)
mostFrequent5Gram words =
  fromJust
    (lookup (maximum (map snd (toList (5gramFrequencyMap words)))) (5gramFrequencyMap words))

-- Function to count the frequency of each 5-gram in a list of words.
5gramFrequencyMap :: [Word] -> Map (Word, Word, Word, Word, Word) Int
5gramFrequencyMap words =
  fromList
    (map (\((w1, w2, w3, w4, w5), n) -> ((w1, w2, w3, w4, w5), n))
      (groupBy ((==) `on` toUpper)
        (sortBy (\(w1, w2, w3, w4, w5) (w6, w7, w8, w9, w10) -> compare (w1, w2, w3, w4, w5) (w6, w7, w8, w9, w10))
          (zip words (tail words) (tail (tail words)) (tail (tail (tail words))) (tail (tail (tail (tail words)))))))))

-- Function to find the least frequent 5-gram in a list of words.
leastFrequent5Gram :: [Word] -> (Word, Word, Word, Word, Word)
leastFrequent5Gram words =
  fromJust
    (lookup (minimum (map snd (toList (5gramFrequencyMap words)))) (5gramFrequencyMap words))
```

This code is a collection of functions for performing various text analysis tasks on a list of words.