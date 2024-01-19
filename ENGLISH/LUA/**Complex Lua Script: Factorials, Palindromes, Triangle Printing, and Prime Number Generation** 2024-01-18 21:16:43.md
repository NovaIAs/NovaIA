```lua
-- This Lua code is a complex and differentiated script that performs various operations. It uses multiple functions and loops to achieve its purpose.

function factorial(n)
    -- Calculates the factorial of a non-negative integer n.
    if n == 0 then
        return 1
    else
        return n * factorial(n-1)
    end
end

function is_palindrome(word)
    -- Checks if a given word is a palindrome (reads the same forwards and backwards).
    local reversed_word = ""
    for i = #word, 1, -1 do
        reversed_word = reversed_word .. string.sub(word, i, i)
    end
    return word == reversed_word
end

function print_triangle(n)
    -- Prints a right triangle of height n using asterisks.
    for i = 1, n do
        for j = 1, i do
            io.write("*")
        end
        io.write("\n")
    end
end

function find_primes(n)
    -- Finds and returns an array of prime numbers up to n.
    local primes = {}
    for i = 2, n do
        local is_prime = true
        for j = 2, math.floor(math.sqrt(i)) do
            if i % j == 0 then
                is_prime = false
                break
            end
        end
        if is_prime then
            table.insert(primes, i)
        end
    end
    return primes
end

-- Demonstrate the usage of the functions defined above.

-- Calculate and print factorials of 0, 5, and 10.
print("Factorials:")
print("0!: ", factorial(0))
print("5!: ", factorial(5))
print("10!: ", factorial(10))

-- Check if "racecar" and "kayak" are palindromes.
print("\nPalindromes:")
print("racecar: ", is_palindrome("racecar"))
print("kayak: ", is_palindrome("kayak"))

-- Print a right triangle of height 5.
print("\nTriangle:")
print_triangle(5)

-- Find and print prime numbers up to 100.
print("\nPrime Numbers:")
local primes = find_primes(100)
for _, prime in pairs(primes) do
    io.write(prime, " ")
end
print()

```
**Explanation:**

1. `factorial` Function: Calculates the factorial of a non-negative integer using recursion.
2. `is_palindrome` Function: Checks if a given word is a palindrome by reversing it and comparing it to the original word.
3. `print_triangle` Function: Prints a right triangle of height `n` using asterisks.
4. `find_primes` Function: Finds and returns an array of prime numbers up to `n` using the Sieve of Eratosthenes algorithm.
5. Demonstration: The script demonstrates the usage of these functions by calculating factorials, checking palindromes, printing a triangle, and finding prime numbers.