```lua

-- Fibonacci sequence calculator

-- Declare a function to calculate the Fibonacci sequence
function fib(n)
    -- Base cases:
    if n == 0 then
        return 0
    elseif n == 1 then
        return 1
    end

    -- Recursive case:
    return fib(n-1) + fib(n-2)
end

-- Print the first 10 Fibonacci numbers
for i = 1, 10 do
    print(fib(i))
end

-- Prime number checker

-- Declare a function to check if a number is prime
function isPrime(n)
    -- If n is less than or equal to 1, it is not prime
    if n <= 1 then
        return false
    end

    -- Iterate over all numbers from 2 to the square root of n
    for i = 2, math.sqrt(n) do
        -- If n is divisible by any number from 2 to its square root, it is not prime
        if n % i == 0 then
            return false
        end
    end

    -- If no divisors were found, n is prime
    return true
end

-- Print the first 10 prime numbers
for i = 2, 100 do
    if isPrime(i) then
        print(i)
    end
end

-- Factorial calculator

-- Declare a function to calculate the factorial of a number
function factorial(n)
    -- Base case:
    if n == 0 then
        return 1
    end

    -- Recursive case:
    return n * factorial(n-1)
end

-- Print the factorial of the first 10 numbers
for i = 1, 10 do
    print(factorial(i))
end

-- Greatest common divisor calculator

-- Declare a function to calculate the greatest common divisor of two numbers
function gcd(a, b)
    -- Base case:
    if b == 0 then
        return a
    end

    -- Recursive case:
    return gcd(b, a % b)
end

-- Print the greatest common divisor of the first 10 pairs of numbers
for i = 1, 10 do
    for j = 1, 10 do
        print(gcd(i, j))
    end
end

-- Least common multiple calculator

-- Declare a function to calculate the least common multiple of two numbers
function lcm(a, b)
    return (a * b) / gcd(a, b)
end

-- Print the least common multiple of the first 10 pairs of numbers
for i = 1, 10 do
    for j = 1, 10 do
        print(lcm(i, j))
    end
end

```

Explanation:



The code above implements several mathematical functions in Lua, including the Fibonacci sequence calculator, the prime number checker, the factorial calculator, the greatest common divisor calculator, and the least common multiple calculator.


The Fibonacci sequence calculator uses recursion to calculate the Fibonacci sequence, which is a series of numbers where each number is the sum of the two preceding ones.


The prime number checker uses a loop to check if a number is prime, which means that it is only divisible by 1 and itself.


The factorial calculator uses recursion to calculate the factorial of a number, which is the product of all positive integers up to that number.


The greatest common divisor calculator uses a recursive algorithm to calculate the greatest common divisor of two numbers, which is the largest number that divides both numbers evenly.


The least common multiple calculator uses the greatest common divisor to calculate the least common multiple of two numbers, which is the smallest number that is divisible by both numbers.