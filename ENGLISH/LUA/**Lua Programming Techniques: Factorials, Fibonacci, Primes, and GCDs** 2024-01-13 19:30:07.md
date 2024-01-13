```lua
-- This is a complex Lua script that demonstrates various programming techniques.

-- Define a custom function to calculate the factorial of a number.
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n-1)
    end
end

-- Create a table to store the first 10 factorial values.
factorials = {}
for i = 0, 9 do
    factorials[i] = factorial(i)
end

-- Print the factorial values in a formatted table.
print("Factorial Values:")
print("-----------------")
for i, value in ipairs(factorials) do
    print(string.format("%2d: %10d", i, value))
end

-- Define a recursive function to calculate the Fibonacci sequence.
function fibonacci(n)
    if n == 0 or n == 1 then
        return n
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

-- Create a table to store the first 10 Fibonacci numbers.
fibonaccis = {}
for i = 0, 9 do
    fibonaccis[i] = fibonacci(i)
end

-- Print the Fibonacci numbers in a formatted table.
print("\nFibonacci Numbers:")
print("-----------------")
for i, value in ipairs(fibonaccis) do
    print(string.format("%2d: %10d", i, value))
end

-- Define a function to check if a number is prime.
function is_prime(n)
    if n <= 1 then
        return false
    end
    for i = 2, math.floor(math.sqrt(n)) do
        if n % i == 0 then
            return false
        end
    end
    return true
end

-- Create a table to store the first 10 prime numbers.
primes = {}
prime_count = 0
i = 2
while prime_count < 10 do
    if is_prime(i) then
        primes[prime_count] = i
        prime_count = prime_count + 1
    end
    i = i + 1
end

-- Print the prime numbers in a formatted table.
print("\nPrime Numbers:")
print("-----------------")
for i, value in ipairs(primes) do
    print(string.format("%2d: %10d", i, value))
end

-- Define a function to compute the greatest common divisor (GCD) of two numbers.
function gcd(a, b)
    if b == 0 then
        return a
    else
        return gcd(b, a % b)
    end
end

-- Create a table to store the GCD of pairs of numbers from 1 to 10.
gcds = {}
for i = 1, 10 do
    for j = 1, 10 do
        gcds[i][j] = gcd(i, j)
    end
end

-- Print the GCD values in a formatted table.
print("\nGreatest Common Divisors:")
print("-------------------------")
print("   | 1 2 3 4 5 6 7 8 9 10")
print("---+------------------------")
for i, row in ipairs(gcds) do
    print(string.format("%2d |", i), table.concat(row, " "))
end
```

This code demonstrates various programming techniques in Lua, including:

* Defining and using custom functions
* Recursion
* Iterative loops
* Table manipulation
* String formatting
* Mathematical operations
* Conditional statements