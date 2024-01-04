```lua
-- This Lua script demonstrates advanced programming techniques, including recursion, dynamic programming, and object-oriented programming. It calculates Fibonacci numbers using dynamic programming and stores the results in a memoization table for efficient retrieval.

-- Define a function to calculate Fibonacci numbers using recursion.
function fib_recursive(n)
    if n <= 1 then
        return n
    else
        return fib_recursive(n-1) + fib_recursive(n-2)
    end
end

-- Define a function to calculate Fibonacci numbers using dynamic programming.
function fib_dynamic(n)
    -- Create a memoization table to store previously calculated Fibonacci numbers.
    local memo = {}

    -- Initialize the memoization table with base cases.
    memo[0] = 0
    memo[1] = 1

    -- Calculate Fibonacci numbers up to the given value.
    for i = 2, n do
        -- Check if the Fibonacci number has already been calculated.
        if memo[i] == nil then
            -- Calculate the Fibonacci number and store it in the memoization table.
            memo[i] = memo[i-1] + memo[i-2]
        end
    end

    -- Return the Fibonacci number at the given index.
    return memo[n]
end

-- Define a class to represent a Fibonacci sequence.
local FibonacciSequence = {}

-- Initialize the Fibonacci sequence with the first two numbers.
FibonacciSequence[0] = 0
FibonacciSequence[1] = 1

-- Define a method to calculate the next Fibonacci number in the sequence.
function FibonacciSequence:next()
    -- Calculate the next Fibonacci number using the previous two numbers.
    local next_number = self[self[#self] - 1] + self[self[#self] - 2]

    -- Add the next Fibonacci number to the sequence.
    self[#self + 1] = next_number

    -- Return the next Fibonacci number.
    return next_number
end

-- Create an instance of the Fibonacci sequence.
local fib_sequence = FibonacciSequence()

-- Calculate and print the first 10 Fibonacci numbers using recursion.
print("Fibonacci numbers using recursion:")
for i = 0, 9 do
    print(fib_recursive(i))
end

-- Calculate and print the first 10 Fibonacci numbers using dynamic programming.
print("Fibonacci numbers using dynamic programming:")
for i = 0, 9 do
    print(fib_dynamic(i))
end

-- Calculate and print the first 10 Fibonacci numbers using the Fibonacci sequence object.
print("Fibonacci numbers using the Fibonacci sequence object:")
for i = 0, 9 do
    print(fib_sequence:next())
end
```

Explanation:

1. **Recursive Fibonacci Function (`fib_recursive`):** This function calculates Fibonacci numbers using recursion. It checks if `n` is 0 or 1 (base cases) and returns `n` in those cases. Otherwise, it recursively calls itself with `n-1` and `n-2` and returns the sum of those results.

2. **Dynamic Programming Fibonacci Function (`fib_dynamic`):** This function calculates Fibonacci numbers using dynamic programming. It creates a memoization table (`memo`) to store previously calculated Fibonacci numbers. It initializes the table with the base cases (`memo[0] = 0` and `memo[1] = 1`). Then, it iterates from 2 to `n`, checking if the Fibonacci number at each index (`i`) has been calculated. If not, it calculates it and stores it in the memoization table. Finally, it returns the Fibonacci number at the given index.

3. **Fibonacci Sequence Class (`FibonacciSequence`):** This class represents a Fibonacci sequence. It is initialized with the first two Fibonacci numbers (`0` and `1`). It has a method called `next()` that calculates the next Fibonacci number in the sequence by adding the last two numbers in the sequence, and returns that number.

4. **Usage:** The script demonstrates the use of these functions and the Fibonacci sequence class by calculating and printing the first 10 Fibonacci numbers using each method.

This code showcases a combination of recursive and dynamic programming techniques, as well as object-oriented programming principles, making it a complex and varied example of Lua programming.