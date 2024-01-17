```haskell
-- Fibonacci Sequence (Recursive)
fib :: Int -> Int
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

-- Factorial (Recursive)
factorial :: Int -> Int
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- Prime Number Checking (Recursive)
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = helper n 2
    where
      helper :: Int -> Int -> Bool
      helper n d
        | d * d > n = True
        | n `mod` d == 0 = False
        | otherwise = helper n (d + 1)

-- Reverse a String (Recursive)
reverseString :: String -> String
reverseString "" = ""
reverseString (x:xs) = reverseString xs ++ [x]

-- Palindrome Checking (Recursive)
isPalindrome :: String -> Bool
isPalindrome "" = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

-- Binary Search (Recursive)
binarySearch :: [Int] -> Int -> Int
binarySearch xs x
  | null xs = -1
  | otherwise = helper xs x 0 (length xs - 1)
    where
      helper :: [Int] -> Int -> Int -> Int -> Int
      helper [] _ _ _ = -1
      helper (x:xs) x l h
        | x == mid = l
        | x < mid = helper xs x l (mid - 1)
        | otherwise = helper xs x (mid + 1) h
          where mid = (l + h) `div` 2

-- Sorting a List (Merge Sort)
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take half xs)) (mergeSort (drop half xs))
  where
    half = length xs `div` 2
    merge :: [Int] -> [Int] -> [Int]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x < y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Generating Permutations
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (insert x) (permutations xs)
  where
    insert :: a -> [a] -> [[a]]
    insert x [] = [[x]]
    insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)
```

**Explanation:**

This Haskell code provides a collection of commonly used and fundamental programming algorithms and functions, including:

1. **Fibonacci Sequence (Recursive):** Calculates the nth Fibonacci number using a recursive approach.

2. **Factorial (Recursive):** Computes the factorial of a given integer n.

3. **Prime Number Checking (Recursive):** Checks whether a given integer is prime or not.

4. **Reverse a String (Recursive):** Reverses a given string.

5. **Palindrome Checking (Recursive):** Determines if a given string is a palindrome or not.

6. **Binary Search (Recursive):** Searches for a target element in a sorted list using a recursive binary search algorithm.

7. **Sorting a List (Merge Sort):** Sorts a list of integers in ascending order using the Merge Sort algorithm.

8. **Generating Permutations:** Generates all possible permutations of a given list.

These functions are implemented using recursion, which is a fundamental programming technique in Haskell. The code also demonstrates how to define helper functions for complex algorithms.