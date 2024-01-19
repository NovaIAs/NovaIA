```haskell
-- This Haskell code defines a function called 'factorial' that calculates the factorial of a given number using a recursive approach.

-- The 'factorial' function takes a single argument 'n' of type 'Int', representing the number for which we want to find the factorial.

factorial :: Int -> Int

-- The recursive implementation of the 'factorial' function is defined as follows:

-- If 'n' is equal to 0, the function returns 1. This serves as the base case for the recursion.
factorial 0 = 1

-- Otherwise, the function calculates the factorial of 'n' by multiplying 'n' with the factorial of '(n-1)'. This recursive call is made until the base case is reached.
factorial n = n * factorial (n-1)

-- The 'main' function is the entry point of the program. It defines an 'IO' action, which represents an input/output operation.

main :: IO ()

-- In the 'main' function, we use the 'putStrLn' function to display a message on the standard output (terminal).
-- The message prompts the user to enter a number to calculate the factorial.
main = putStrLn "Enter a number to calculate the factorial: "

-- We use the 'getLine' function to read a single line of text from the standard input (terminal). The user's input is stored in the 'input' variable.
input <- getLine

-- We use the 'read' function to convert the user's input (a string) into an 'Int' value.
-- The result is stored in the 'number' variable.
number <- read input :: Int

-- We call the 'factorial' function with the 'number' as an argument and store the result in the 'result' variable.
result <- factorial number

-- Finally, we use the 'putStrLn' function to display the calculated factorial of the number on the standard output.
putStrLn ("The factorial of " ++ show number ++ " is " ++ show result)
```

**Explanation:**

1. **'factorial' Function:**
   - This function takes an integer 'n' as input and returns the factorial of 'n'.
   - Factorial of a number 'n' is the product of all positive integers from 1 to 'n'.
   - The function uses a recursive approach to calculate the factorial.
   - The base case is when 'n' is equal to 0, in which case the function returns 1.
   - For other values of 'n', the function multiplies 'n' with the factorial of '(n-1)' and returns the result.

2. **'main' Function:**
   - This function defines the entry point of the program.
   - It uses the 'putStrLn' function to display a message prompting the user to enter a number.
   - It reads the user's input using 'getLine' and converts it to an 'Int' value using 'read'.
   - It calls the 'factorial' function with the user-entered number and stores the result in the 'result' variable.
   - Finally, it uses 'putStrLn' to display the calculated factorial on the standard output.

This code provides a complete and functional program that calculates the factorial of a number entered by the user. It combines the concepts of recursion, input/output operations, and data conversion to achieve this task.