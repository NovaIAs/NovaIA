```haskell
module VarArgs where

import Data.List

vSum :: Num a => [a] -> a
vSum = foldr (+) 0

-- Function to check the parity of a number.
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- Function to check if a list has any even numbers.
anyEven :: [Int] -> Bool
anyEven = any isEven

-- Function to check if a list has all even numbers.
allEven :: [Int] -> Bool
allEven = all isEven

main :: IO ()
main = do
  putStrLn "Enter a list of numbers, separated by spaces: "
  input <- getLine
  let numbers = map read (words input) :: [Int]
  putStrLn ("Sum of the numbers: " ++ show (vSum numbers))
  putStrLn ("Any even numbers in the list: " ++ show (anyEven numbers))
  putStrLn ("All numbers in the list are even: " ++ show (allEven numbers))
```

This Haskell code defines a module named `VarArgs` that provides functions to work with variable-length argument lists, as well as functions to check the parity of a number and a list of numbers. The code also includes a `main` function that prompts the user to enter a list of numbers, then calculates and prints the sum of the numbers, checks if any of the numbers are even, and checks if all the numbers are even.

A step-by-step explanation of the code:

1. Import Necessary Libraries:

   ```haskell
   import Data.List
   ```

   This line imports the `Data.List` module, which provides various functions for working with lists in Haskell.

2. Define Variable-Length Argument Function:

   ```haskell
   vSum :: Num a => [a] -> a
   vSum = foldr (+) 0
   ```

   This defines a function named `vSum` that takes a list of values (`[a]`) and returns their sum. It uses the `foldr` function along with the `(+)` operator to accumulate the sum of the values in the list.

3. Define Helper Functions for Parity Checking:

   ```haskell
   isEven :: Int -> Bool
   isEven n = n `mod` 2 == 0
   ```

   This defines a function named `isEven` that takes an integer `n` and returns `True` if `n` is even, and `False` otherwise. It uses the modulo operator (`mod`) to check if `n` is divisible by 2.

   ```haskell
   anyEven :: [Int] -> Bool
   anyEven = any isEven
   ```

   This defines a function named `anyEven` that takes a list of integers `[Int]` and returns `True` if any of the elements in the list are even. It uses the `any` function along with the `isEven` function to check for any even numbers in the list.

   ```haskell
   allEven :: [Int] -> Bool
   allEven = all isEven
   ```

   This defines a function named `allEven` that takes a list of integers `[Int]` and returns `True` if all the elements in the list are even. It uses the `all` function along with the `isEven` function to check if all elements in the list are even.

4. Define the `main` Function:

   ```haskell
   main :: IO ()
   main = do
   ```

   This defines the `main` function, which is the entry point of the program.

   ```haskell
   putStrLn "Enter a list of numbers, separated by spaces: "
   ```

   This line prompts the user to enter a list of numbers, separated by spaces.

   ```haskell
   input <- getLine
   ```

   This line reads the user's input as a string and stores it in the variable `input`.

   ```haskell
   let numbers = map read (words input) :: [Int]
   ```

   This line splits the input string into a list of words (the numbers entered by the user), converts each word to an integer using the `read` function, and stores the resulting list of integers in the variable `numbers`.

   ```haskell
   putStrLn ("Sum of the numbers: " ++ show (vSum numbers))
   ```

   This line calculates the sum of the numbers using the `vSum` function and prints the result along with a message.

   ```haskell
   putStrLn ("Any even numbers in the list: " ++ show (anyEven numbers))
   ```

   This line checks if any of the numbers in the list are even using the `anyEven` function and prints the result along with a message.

   ```haskell
   putStrLn ("All numbers in the list are even: " ++ show (allEven numbers))
   ```

   This line checks if all the numbers in the list are even using the `allEven` function and prints the result along with a message.