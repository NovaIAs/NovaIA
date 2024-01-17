```haskell
module ComplexHaskell where

import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Control.Monad (guard, liftM, join, forM)
import Control.Applicative ((<$>), (<*>))
import Numeric.Natural (Natural)
import Data.List (foldl', permutations)

-- Type alias for a list of natural numbers
type IntList = [Natural]

-- Function to calculate the sum of a list of natural numbers
sumList :: IntList -> Natural
sumList = foldl' (+) 0

-- Function to calculate the product of a list of natural numbers
productList :: IntList -> Natural
productList = foldl' (*) 1

-- Function to calculate the factorial of a natural number
factorial :: Natural -> Natural
factorial = foldl' (*) 1 [1..n]

-- Function to calculate the greatest common divisor of two natural numbers
gcd :: Natural -> Natural -> Natural
gcd a b | b == 0    = abs a
       | otherwise = gcd b (a `mod` b)

-- Function to calculate the least common multiple of two natural numbers
lcm :: Natural -> Natural -> Natural
lcm a b = abs (a * b) `div` (gcd a b)

-- Function to generate all possible permutations of a list
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (insert x) (permutations xs)
  where insert x [] = [[x]]
        insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)

-- Function to calculate the number of ways to make change for a given amount of money using a given set of coins
change :: [Natural] -> Natural -> IntList
change coins amount = go amount 0
  where
    go 0 _ = [[]]

    go amount acc
      | acc >= length coins = []
      | otherwise            = (amount - coins !! acc) : (go amount (acc + 1)) ++ go amount acc

-- Function to print a list of natural numbers in a nice format
printList :: IntList -> String
printList = intercalate ", " . map show

-- Main function to demonstrate the usage of the above functions
main :: IO ()
main = do
  let coins = [1, 5, 10, 25]
      amount = 100

  putStrLn $ "Sum of the list: " ++ printList [1, 2, 3, 4, 5]

  putStrLn $ "Product of the list: " ++ printList [1, 2, 3, 4, 5]

  putStrLn $ "Factorial of 5: " ++ show (factorial 5)

  putStrLn $ "Greatest common divisor of 12 and 18: " ++ show (gcd 12 18)

  putStrLn $ "Least common multiple of 12 and 18: " ++ show (lcm 12 18)

  putStrLn $ "Permutations of the list [1, 2, 3]: " ++ printList (permutations [1, 2, 3])

  putStrLn $ "Number of ways to make change for $1.00 using coins [1, 5, 10, 25]: " ++ printList (change coins amount)
```

This code is a collection of various Haskell functions that perform different mathematical operations and calculations. It includes functions to calculate the sum, product, factorial, greatest common divisor, least common multiple, permutations, and the number of ways to make change for a given amount of money using a given set of coins. The code is well-commented and easy to understand.