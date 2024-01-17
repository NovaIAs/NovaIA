```lua
-- Define a function to calculate the factorial of a number
function factorial(n)
    -- If the input is less than 0, return 0 (factorial of a negative number is undefined)
    if n < 0 then
        return 0
    elseif n == 0 then  -- Base case: factorial of 0 is 1
        return 1
    else
        -- Recursively calculate the factorial by multiplying n with the factorial of (n-1)
        return n * factorial(n-1)
    end
end

-- Define a function to check if a number is prime
function is_prime(n)
    -- If the number is less than or equal to 1, it is not prime
    if n <= 1 then
        return false
    end

    -- Check if the number is divisible by any number from 2 to the square root of the number
    for i = 2, math.sqrt(n) do
        if n % i == 0 then
            return false  -- If the number is divisible by any number other than 1 and itself, it is not prime
        end
    end

    -- If the number is not divisible by any number from 2 to its square root, it is prime
    return true
end

-- Define a function to find the Fibonacci sequence up to a given number of terms
function fibonacci(n)
    -- Initialize the first two terms of the Fibonacci sequence
    local a = 0
    local b = 1

    -- Create an empty table to store the Fibonacci sequence
    local fib_sequence = {}

    -- Iterate from 1 to the input number
    for i = 1, n do
        -- Add the current Fibonacci number to the sequence
        fib_sequence[i] = a

        -- Calculate the next Fibonacci number by adding the previous two numbers
        local temp = a
        a = b
        b = temp + b
    end

    -- Return the Fibonacci sequence as a table
    return fib_sequence
end

-- Define a function to print a right-aligned table
function print_table(table)
    -- Find the maximum length of the elements in the table
    local max_length = 0
    for _, element in pairs(table) do
        max_length = math.max(max_length, #element)
    end

    -- Iterate over the table and print each element right-aligned with the specified width
    for _, element in pairs(table) do
        print(string.rep(" ", max_length - #element) .. element)
    end
end

-- Print the factorial of numbers from 1 to 10
print("Factorials:")
for n = 1, 10 do
    print(string.format("%2d! = %d", n, factorial(n)))
end

-- Print a list of prime numbers from 1 to 100
print("\nPrime Numbers:")
for n = 1, 100 do
    if is_prime(n) then
        print(n, ", ")
    end
end

-- Print the Fibonacci sequence up to 20 terms
print("\n\nFibonacci Sequence:")
print_table(fibonacci(20))
```

Explanation:

1. **Factorial Function:**

   - `factorial()` calculates the factorial of a number using recursion. If the input is negative or zero, it returns 0 or 1, respectively. Otherwise, it multiplies the number with the factorial of the previous number.

2. **Is Prime Function:**

   - `is_prime()` checks if a number is prime. It returns `false` if the number is less than or equal to 1. It then iteratively checks if the number is divisible by any number from 2 to the square root of the number. If it finds a divisor, it returns `false`. Otherwise, it returns `true`.

3. **Fibonacci Function:**

   - `fibonacci()` generates the Fibonacci sequence up to a specified number of terms. It initializes the first two terms as 0 and 1 and then iteratively calculates the next term by adding the previous two terms. The resulting sequence is returned as a table.

4. **Print Table Function:**

   - `print_table()` is a utility function used to print a table in a right-aligned format. It finds the maximum length of the elements in the table and then prints each element with the specified width.

5. **Printing Results:**

   - The script calculates and prints the factorial of numbers from 1 to 10, a list of prime numbers from 1 to 100, and the Fibonacci sequence up to 20 terms. It uses the `print_table()` function to format the output neatly.