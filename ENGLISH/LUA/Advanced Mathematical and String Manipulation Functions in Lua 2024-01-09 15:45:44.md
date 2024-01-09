```lua
-- Require the necessary libraries
local math = require "math"
local string = require "string"
local table = require "table"

-- Define a custom function to calculate the factorial of a number
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Define a custom function to check if a number is prime
function is_prime(n)
  if n <= 1 then
    return false
  elseif n == 2 then
    return true
  else
    for i = 2, math.floor(math.sqrt(n)) do
      if n % i == 0 then
        return false
      end
    end
    return true
  end
end

-- Define a custom function to find all the prime factors of a number
function prime_factors(n)
  local factors = {}
  local divisor = 2
  while n > 1 do
    if n % divisor == 0 then
      table.insert(factors, divisor)
      n = n / divisor
    else
      divisor = divisor + 1
    end
  end
  return factors
end

-- Define a custom function to generate a random number between two values
function random_number(min, max)
  return math.floor(math.random() * (max - min + 1)) + min
end

-- Define a custom function to generate a random string of a given length
function random_string(length)
  local chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  local str = ""
  for i = 1, length do
    str = str .. chars:sub(random_number(1, #chars), random_number(1, #chars))
  end
  return str
end

-- Define a custom function to sort a table of numbers in ascending order
function sort_numbers(table)
  table.sort(function(a, b) return a < b end)
end

-- Define a custom function to sort a table of strings in alphabetical order
function sort_strings(table)
  table.sort(function(a, b) return a < b end)
end

-- Define a custom function to print a table of values in a human-readable format
function print_table(table)
  for i, v in ipairs(table) do
    print(i .. ": " .. v)
  end
end

-- Create a table of numbers
local numbers = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19}

-- Print the table of numbers
print_table(numbers)

-- Sort the table of numbers in ascending order
sort_numbers(numbers)

-- Print the sorted table of numbers
print_table(numbers)

-- Create a table of strings
local strings = {"apple", "banana", "cherry", "durian", "elderberry", "fig", "grape", "honeydew", "ice cream", "jackfruit"}

-- Print the table of strings
print_table(strings)

-- Sort the table of strings in alphabetical order
sort_strings(strings)

-- Print the sorted table of strings
print_table(strings)

-- Generate a random number between 1 and 10
local random_num = random_number(1, 10)

-- Print the random number
print("Random number:", random_num)

-- Generate a random string of length 10
local random_str = random_string(10)

-- Print the random string
print("Random string:", random_str)

-- Calculate the factorial of 10
local factorial_10 = factorial(10)

-- Print the factorial of 10
print("Factorial of 10:", factorial_10)

-- Check if 17 is a prime number
local is_prime_17 = is_prime(17)

-- Print whether 17 is a prime number
print("Is 17 prime?", is_prime_17)

-- Find the prime factors of 100
local prime_factors_100 = prime_factors(100)

-- Print the prime factors of 100
print_table(prime_factors_100)
```
**Explanation:**

This Lua code is a collection of custom functions and examples that demonstrate various mathematical and string manipulation operations. It includes functions to calculate the factorial of a number, check if a number is prime, find the prime factors of a number, generate random numbers and strings, sort tables of numbers and strings, and print tables in a human-readable format.

The code also provides examples of using these functions to perform various operations on tables of numbers and strings, such as sorting them in ascending order or alphabetical order, calculating the factorial of a number, checking if a number is prime, finding the prime factors of a number, and generating random numbers and strings.

Overall, this code showcases a variety of useful functions and operations that can be performed in Lua, making it a valuable resource for Lua programmers.