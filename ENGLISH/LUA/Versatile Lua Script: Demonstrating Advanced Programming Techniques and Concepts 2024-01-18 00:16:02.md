```lua
-- This Lua script is a complex and highly differentiated program that demonstrates various programming concepts and techniques.

-- 1. Function to calculate the factorial of a number using recursive programming:
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end

-- 2. Use of the "for" loop to iterate through a range of numbers:
for i = 1, 10 do
  print("Factorial of " .. i .. " is " .. factorial(i))
end

-- 3. Create a table (array) of strings and perform various operations on it:
local names = {"John", "Mary", "Bob", "Alice", "Tom"}

-- Add a new element to the end of the table:
table.insert(names, "Susan")

-- Remove the first element from the table:
table.remove(names, 1)

-- Print the contents of the table:
for i, name in ipairs(names) do
  print("Name " .. i .. ": " .. name)
end

-- 4. Use the "while" loop to continuously prompt the user for input until they enter "quit":
while true do
  local input = io.read()
  if input == "quit" then
    break
  else
    print("You entered: " .. input)
  end
end

-- 5. Define a function that takes two numbers and returns their greatest common divisor using the Euclidean algorithm:
function gcd(a, b)
  while b ~= 0 do
    a, b = b, a % b
  end
  return a
end

-- 6. Define a function to check if a number is prime using trial division:
function isprime(n)
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

-- 7. Use the "string" library to manipulate strings:
local text = "This is a sample text."

-- Find the first occurrence of the letter 's' in the string:
local position = string.find(text, "s")

-- Replace all occurrences of 'sample' with 'example':
local new_text = string.gsub(text, "sample", "example")

-- Convert the string to uppercase:
local upper_text = string.upper(text)

-- 8. Use the "math" library to perform mathematical operations:
local result = math.sqrt(16) -- Square root of 16
result = math.sin(math.pi / 2) -- Sine of pi/2

-- 9. Use the "os" library to interact with the operating system:
local filename = "test.txt"
local file = io.open(filename, "w") -- Open the file for writing

file:write("Hello from Lua!\n") -- Write a line of text to the file

file:close() -- Close the file

-- 10. Use the "table" library to sort a table of numbers:
local numbers = {5, 3, 1, 2, 4}

table.sort(numbers) -- Sort the table in ascending order

for i, num in ipairs(numbers) do
  print("Number " .. i .. ": " .. num)
end

-- This code demonstrates various programming concepts such as recursion, iteration, table manipulation, input/output operations, mathematical calculations, string manipulation, file handling, and sorting. The explanations provided with each section help understand the purpose and functionality of each code block.
```