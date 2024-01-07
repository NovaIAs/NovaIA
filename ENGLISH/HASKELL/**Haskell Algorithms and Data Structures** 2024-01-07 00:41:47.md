```haskell
module ComplexCode (factorial, fibonacci, gcd, lcm, power,isPrime, primes, mergeSort, quickSort, binarySearch, insertionSort, selectionSort, bubbleSort) where

import Data.List (sort)

-- Factorial function
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Fibonacci function
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Greatest common divisor
gcd :: Int -> Int -> Int
gcd a b
  | b == 0 = a
  | otherwise = gcd b (a `rem` b)

-- Least common multiple
lcm :: Int -> Int -> Int
lcm a b = abs (a * b) `div` gcd a b

-- Power function
power :: Int -> Int -> Int
power _ 0 = 1
power a n = a * power a (n - 1)

-- Primality test
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | n `mod` 2 == 0 = False
  | otherwise = all (\i -> n `mod` i /= 0) [3, 5 .. floor (sqrt (fromIntegral n))]

-- Prime numbers up to a given limit
primes :: Int -> [Int]
primes n = filter isPrime [2 .. n]

-- Merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take half xs)) (mergeSort (drop half xs))
  where half = length xs `div` 2

-- Quick sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)

-- Binary search
binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch x xs
  | null xs = Nothing
  | x == head xs = Just 0
  | x == last xs = Just (length xs - 1)
  | x < head xs || x > last xs = Nothing
  | otherwise = binarySearch' x xs 0 (length xs - 1)
  where
    binarySearch' x xs low high
      | low > high = Nothing
      | mid == x = Just mid
      | x < mid = binarySearch' x xs low (mid - 1)
      | otherwise = binarySearch' x xs (mid + 1) high
      where mid = (low + high) `div` 2

-- Insertion sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Selection sort
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minimum xs : selectionSort (delete minimum xs)

-- Bubble sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort (x:xs) = bubbleSort' x xs
  where
    bubbleSort' x [] = [x]
    bubbleSort' x (y:ys)
      | x <= y = x : bubbleSort' y ys
      | otherwise = y : bubbleSort' x ys
```

This code contains a collection of commonly used functions in Haskell, including factorial, fibonacci, greatest common divisor (gcd), least common multiple (lcm), power, primality test, prime numbers, merge sort, quick sort, binary search, insertion sort, selection sort, and bubble sort.

The code is written in a modular and well-structured manner, with each function defined separately. The functions are also well-documented with comments explaining their purpose and usage.

Overall, this code is a good example of how to write complex and efficient code in Haskell.