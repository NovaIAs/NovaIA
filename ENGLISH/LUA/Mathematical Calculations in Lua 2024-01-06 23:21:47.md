```lua
function is_prime(n)
    if n <= 1 then
        return false
    end
    for i = 2, math.sqrt(n) do
        if n % i == 0 then
            return false
        end
    end
    return true
end

function sum_primes_up_to(n)
    local sum = 0
    for i = 2, n do
        if is_prime(i) then
            sum = sum + i
        end
    end
    return sum
end

function find_largest_prime_factor(n)
    local largest_prime_factor = 1
    local i = 2
    while i <= n do
        if n % i == 0 and is_prime(i) then
            largest_prime_factor = i
        end
        i = i + 1
    end
    return largest_prime_factor
end

function is_palindrome(n)
    local s = tostring(n)
    local reversed_s = ""
    for i = #s, 1, -1 do
        reversed_s = reversed_s .. s:sub(i, i)
    end
    return s == reversed_s
end

function find_largest_palindrome_product(n)
    local largest_palindrome_product = 0
    for i = 1, n do
        for j = 1, n do
            local product = i * j
            if is_palindrome(product) and product > largest_palindrome_product then
                largest_palindrome_product = product
            end
        end
    end
    return largest_palindrome_product
end

function print_fibonacci_sequence(n)
    local a = 0
    local b = 1
    print(a)
    print(b)
    for i = 3, n do
        local c = a + b
        print(c)
        a = b
        b = c
    end
end

function find_nth_fibonacci_number(n)
    if n == 1 or n == 2 then
        return 1
    end
    return find_nth_fibonacci_number(n - 1) + find_nth_fibonacci_number(n - 2)
end

function find_smallest_positive_integer_divisible_by_all_numbers_up_to(n)
    local smallest_positive_integer = 1
    for i = 2, n do
        local found_divisor = false
        for j = 2, smallest_positive_integer do
            if smallest_positive_integer % j == 0 and i % j == 0 then
                found_divisor = true
                break
            end
        end
        if not found_divisor then
            smallest_positive_integer = smallest_positive_integer * i
        end
    end
    return smallest_positive_integer
end

-- This code calculates the sum of the first 100 prime numbers.
local sum_of_primes = sum_primes_up_to(100)
print("The sum of the first 100 prime numbers is:", sum_of_primes)

-- This code finds the largest prime factor of the number 600851475143.
local largest_prime_factor = find_largest_prime_factor(600851475143)
print("The largest prime factor of 600851475143 is:", largest_prime_factor)

-- This code finds the largest palindrome product of two 3-digit numbers.
local largest_palindrome_product = find_largest_palindrome_product(999)
print("The largest palindrome product of two 3-digit numbers is:", largest_palindrome_product)

-- This code prints the Fibonacci sequence up to the 10th number.
print_fibonacci_sequence(10)

-- This code finds the 10th Fibonacci number.
local tenth_fibonacci_number = find_nth_fibonacci_number(10)
print("The 10th Fibonacci number is:", tenth_fibonacci_number)

-- This code finds the smallest positive integer that is divisible by all numbers up to 20.
local smallest_positive_integer = find_smallest_positive_integer_divisible_by_all_numbers_up_to(20)
print("The smallest positive integer that is divisible by all numbers up to 20 is:", smallest_positive_integer)
```

This code performs a variety of mathematical calculations, including finding the sum of the first 100 prime numbers, finding the largest prime factor of a large number, finding the largest palindrome product of two 3-digit numbers, printing the Fibonacci sequence up to a certain number, finding the nth Fibonacci number, and finding the smallest positive integer that is divisible by all numbers up to a certain number.

The code is well-commented and easy to understand, and it uses a variety of Lua functions and constructs, including loops, conditional statements, and functions.