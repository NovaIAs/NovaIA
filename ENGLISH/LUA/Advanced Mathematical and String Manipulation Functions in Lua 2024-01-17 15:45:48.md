```lua
-- Create a dynamic table of Fibonacci numbers up to a given limit
function fibTable(limit)
    local fib = {0, 1}  -- Initial Fibonacci sequence
    while fib[#fib] < limit do  -- Iterate until the last Fibonacci number exceeds the limit
        local nextFib = fib[#fib] + fib[#fib - 1]  -- Calculate the next Fibonacci number
        table.insert(fib, nextFib)  -- Add the next Fibonacci number to the table
    end
    return fib
end

-- Find the largest prime factor of a given number
function largestPrimeFactor(n)
    local largestPrime = 1  -- Initialize the largest prime factor to 1
    local i = 2  -- Start checking for prime factors from 2
    while i <= n do  -- Iterate through potential prime factors up to the given number
        if n % i == 0 then  -- If the number is divisible by the current potential prime factor
            while n % i == 0 do  -- Keep dividing the number by the prime factor until it's no longer divisible
                n = n / i
            end
            largestPrime = i  -- Update the largest prime factor if this is a larger prime factor
        end
        i = i + 1  -- Move to the next potential prime factor
    end
    return largestPrime
end

-- Calculate the sum of the digits of a given number
function digitSum(n)
    local sum = 0
    while n > 0 do  -- Iterate while there are still digits in the number
        sum = sum + n % 10  -- Add the last digit to the sum
        n = math.floor(n / 10)  -- Remove the last digit from the number
    end
    return sum
end

-- Check if a given number is a palindrome
function isPalindrome(n)
    local reversed = 0
    local original = n
    while n > 0 do  -- Iterate while there are still digits in the number
        reversed = reversed * 10 + n % 10  -- Append the last digit to the reversed number
        n = math.floor(n / 10)  -- Remove the last digit from the original number
    end
    return original == reversed
end

-- Find the greatest common divisor (GCD) of two numbers using the Euclidean algorithm
function gcd(a, b)
    if a == 0 then  -- If 'a' is zero, the GCD is 'b'
        return b
    end
    return gcd(b % a, a)  -- Recursively call gcd with 'b' mod 'a' and 'a' as arguments
end

-- Find the factorial of a given number using recursion
function factorial(n)
    if n == 0 then  -- If 'n' is zero, the factorial is 1
        return 1
    end
    return n * factorial(n - 1)  -- Recursively call factorial with 'n - 1' as the argument
end

-- Check if a given string is a valid email address
function isEmailValid(email)
    local pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}$"  -- Regular expression pattern for email validation
    return email:match(pattern) ~= nil  -- Check if the email matches the pattern
end
```

Explanation:

1. **fibTable**: This function generates a dynamic table of Fibonacci numbers up to a specified limit.
2. **largestPrimeFactor**: It finds the largest prime factor of a given number using trial division.
3. **digitSum**: It calculates the sum of the digits of a given number.
4. **isPalindrome**: This function checks if a given number is a palindrome by comparing it to its reverse.
5. **gcd**: It finds the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.
6. **factorial**: This function calculates the factorial of a given number using recursion.
7. **isEmailValid**: It checks if a given string is a valid email address using a regular expression pattern.

This code covers various mathematical operations, number theory, recursion, pattern matching, and string validation, demonstrating the versatility of the Lua programming language.