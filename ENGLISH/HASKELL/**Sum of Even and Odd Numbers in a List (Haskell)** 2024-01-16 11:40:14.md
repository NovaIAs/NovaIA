```haskell
-- This Haskell code defines a function called 'myFunction' that takes a list of integers as input 
-- and returns a tuple containing the sum of the even numbers in the list and the sum of the odd numbers in the list.

myFunction :: [Int] -> (Int, Int)
myFunction [] = (0, 0) -- If the input list is empty, return a tuple of two zeros.
myFunction (x:xs) =
  let (evenSum, oddSum) = myFunction xs -- Recursively call 'myFunction' on the tail of the input list.
  in
    if x `mod` 2 == 0 -- Check if the current element 'x' is even.
    then (evenSum + x, oddSum) -- If 'x' is even, add it to the sum of even numbers and return.
    else (evenSum, oddSum + x) -- Otherwise, add it to the sum of odd numbers and return.

-- Example usage of the 'myFunction' function.

let inputList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let (evenSum, oddSum) = myFunction inputList -- Call 'myFunction' with the input list.

-- Print the results to the console.

putStrLn ("Sum of even numbers: " ++ show evenSum)
putStrLn ("Sum of odd numbers: " ++ show oddSum)
```

Explanation:

1. **Function Signature**:

   ```haskell
   myFunction :: [Int] -> (Int, Int)
   ```

   - This line defines the signature of the 'myFunction' function. It takes a list of integers (`[Int]`) as input and returns a tuple containing two integers (`(Int, Int)`). The first element of the tuple represents the sum of even numbers in the list, and the second element represents the sum of odd numbers in the list.

2. **Base Case**:

   ```haskell
   myFunction [] = (0, 0)
   ```

   - This is the base case of the recursive function. When the input list is empty (`[]`), it immediately returns a tuple of two zeros. This means that the sum of even numbers and the sum of odd numbers in an empty list are both zero.

3. **Recursive Case**:

   ```haskell
   myFunction (x:xs) =
     let (evenSum, oddSum) = myFunction xs
     in
       if x `mod` 2 == 0
       then (evenSum + x, oddSum)
       else (evenSum, oddSum + x)
   ```

   - This is the recursive case of the function. It handles non-empty input lists.
   - It starts by recursively calling 'myFunction' on the tail of the input list (`xs`). This call calculates the sum of even and odd numbers in the tail of the list.
   - It then checks if the current element 'x' is even using the modulo operator (`x `mod` 2 == 0`). If 'x' is even, it adds 'x' to the sum of even numbers and returns the updated tuple. Otherwise, it adds 'x' to the sum of odd numbers and returns the updated tuple.

4. **Example Usage**:

   ```haskell
   let inputList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
   let (evenSum, oddSum) = myFunction inputList
   ```

   - This code defines an example input list called 'inputList' containing integers from 1 to 10.
   - It then calls 'myFunction' with 'inputList' as an argument and stores the result in the tuple '(evenSum, oddSum)'.

5. **Printing the Results**:

   ```haskell
   putStrLn ("Sum of even numbers: " ++ show evenSum)
   putStrLn ("Sum of odd numbers: " ++ show oddSum)
   ```

   - These lines use the 'putStrLn' function to print the sum of even numbers and the sum of odd numbers to the console. The 'show' function is used to convert the integer values to strings before printing them.

This code demonstrates a recursive function in Haskell that calculates the sum of even and odd numbers in a list of integers. It uses pattern matching and recursion to handle both empty and non-empty lists efficiently.