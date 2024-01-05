```haskell
import Data.List (nub, sort)

-- The core logic of the program.
-- It takes a list of numbers and returns a list of unique primes.
primeFactors :: [Int] -> [Int]
primeFactors ns = nub $ filter isPrime $ concatMap primeFactorsOf ns

-- Check if a number is prime.
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | otherwise = null $ filter ((== 0) . n `mod`) [2..n-1]

-- Get the prime factors of a number.
primeFactorsOf :: Int -> [Int]
primeFactorsOf n
  | n < 2 = []
  | isPrime n = [n]
  | otherwise = primeFactorsOf (head $ filter ((== 0) . n `mod`) [2..n-1]) ++ primeFactorsOf (n `div` (head $ filter ((== 0) . n `mod`) [2..n-1]))

-- The main function.
-- It takes a list of numbers as input and prints the prime factors of each number.
main :: IO ()
main = do
  ns <- map read . lines <$> getContents
  putStrLn $ unlines $ map (show . primeFactors) ns
```

Explanation:

1. The `primeFactors` function takes a list of numbers as input and returns a list of unique prime factors of each number. It first calls the `nub` function to remove any duplicate prime factors, and then filters the list of prime factors using the `isPrime` function to ensure that only prime factors are included. Finally, it calls the `concatMap` function to apply the `primeFactorsOf` function to each number in the input list, concatenating the results into a single list.

2. The `isPrime` function checks if a number is prime by checking if it is less than 2 (in which case it is not prime), if it is equal to 2 (in which case it is prime), or if it is divisible by any number between 2 and itself minus 1 (in which case it is not prime).

3. The `primeFactorsOf` function takes a number as input and returns a list of its prime factors. It first checks if the number is less than 2 (in which case it has no prime factors), if it is prime (in which case it is its own prime factor), or if it is divisible by any number between 2 and itself minus 1. If the number is divisible by a number between 2 and itself minus 1, it recursively calls the `primeFactorsOf` function on the number and the number divided by the divisor, and concatenates the results.

4. The `main` function takes a list of numbers as input from the standard input, and prints the prime factors of each number to the standard output. It first uses the `map` function to convert each line of input to an integer, and then uses the `putStrLn` function to print the prime factors of each number, separated by newlines.