Here's a complex Haskell code that performs various operations on a list of integers:

```haskell
module ComplexHaskell where

import Data.List (partition, sort)
import Data.Function (on)

-- Main function
main :: IO ()
main = do
  -- Define a list of integers
  let numbers = [1, 3, 5, 2, 4, 6]

  -- Print the original list
  putStrLn "Original list:"
  print numbers

  -- Partition the list into even and odd numbers
  let (evenNumbers, oddNumbers) = partition even numbers

  -- Print the partitioned lists
  putStrLn "Even numbers:"
  print evenNumbers
  putStrLn "Odd numbers:"
  print oddNumbers

  -- Sort the list in ascending order
  let sortedNumbers = sort numbers

  -- Print the sorted list
  putStrLn "Sorted list:"
  print sortedNumbers

  -- Find the maximum and minimum values in the list
  let (maximumValue, minimumValue) = (maximum numbers, minimum numbers)

  -- Print the maximum and minimum values
  putStrLn "Maximum value:"
  print maximumValue
  putStrLn "Minimum value:"
  print minimumValue

  -- Calculate the sum and product of the numbers in the list
  let sumOfNumbers = sum numbers
  let productOfNumbers = product numbers

  -- Print the sum and product
  putStrLn "Sum of numbers:"
  print sumOfNumbers
  putStrLn "Product of numbers:"
  print productOfNumbers

  -- Group the numbers into even and odd groups using a higher-order function
  let groupedNumbers = groupBy (\x y -> x `mod` 2 == y `mod` 2) numbers

  -- Print the grouped numbers
  putStrLn "Grouped numbers:"
  print groupedNumbers
```

Explanation:

1. **Module Declaration**: The code starts with the `module` declaration, which specifies the name of the module, `ComplexHaskell`.

2. **Importing Modules**: It imports the `Data.List` and `Data.Function` modules, which provide various list and function-related operations.

3. **Main Function**: The `main` function is the entry point of the program.

4. **Defining the List**: A list of integers named `numbers` is defined.

5. **Printing the Original List**: The original list is printed using `putStrLn` and `print`.

6. **Partitioning the List**: The `partition` function from `Data.List` is used to partition the list into even and odd numbers, resulting in two lists: `evenNumbers` and `oddNumbers`.

7. **Printing the Partitioned Lists**: The partitioned lists are printed.

8. **Sorting the List**: The `sort` function from `Data.List` is used to sort the list in ascending order, resulting in the `sortedNumbers`.

9. **Printing the Sorted List**: The sorted list is printed.

10. **Finding Maximum and Minimum Values**: The `maximum` and `minimum` functions are used to find the maximum and minimum values in the `numbers` list.

11. **Printing Maximum and Minimum Values**: The maximum and minimum values are printed.

12. **Calculating Sum and Product**: The `sum` and `product` functions are used to calculate the sum and product of the numbers in the `numbers` list.

13. **Printing Sum and Product**: The sum and product are printed.

14. **Grouping Numbers**: A higher-order function `groupBy` from `Data.List` is used to group the numbers into even and odd groups based on their remainders when divided by 2. This grouping is stored in the `groupedNumbers` variable.

15. **Printing Grouped Numbers**: The grouped numbers are printed.