```lua
-- Function to calculate the factorial of a number
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Function to check if a number is prime
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

-- Function to find the greatest common divisor of two numbers
function gcd(a, b)
  if b == 0 then
    return a
  end

  return gcd(b, a % b)
end

-- Function to find the least common multiple of two numbers
function lcm(a, b)
  return (a * b) / gcd(a, b)
end

-- Function to generate a random number between two numbers
function random_range(min, max)
  return math.random(min, max)
end

-- Function to generate a random number between two numbers
function random_choice(choices)
  return choices[math.random(#choices)]
end

-- Function to shuffle an array
function shuffle(array)
  for i = 1, #array do
    local j = math.random(i, #array)
    local temp = array[i]
    array[i] = array[j]
    array[j] = temp
  end

  return array
end

-- Function to sort an array
function sort(array)
  table.sort(array, function(a, b) return a < b end)

  return array
end

-- Function to reverse an array
function reverse(array)
  local new_array = {}

  for i = #array, 1, -1 do
    new_array[#new_array + 1] = array[i]
  end

  return new_array
end

-- Function to find the index of an element in an array
function find_index(array, element)
  for i = 1, #array do
    if array[i] == element then
      return i
    end
  end

  return -1
end

-- Function to remove an element from an array
function remove_element(array, element)
  local index = find_index(array, element)

  if index ~= -1 then
    table.remove(array, index)
  end

  return array
end

-- Function to insert an element into an array at a specific index
function insert_element(array, element, index)
  table.insert(array, index, element)

  return array
end

-- Function to convert a number to a string
function to_string(number)
  return string.format("%d", number)
end

-- Function to convert a string to a number
function to_number(string)
  return tonumber(string)
end

-- Function to convert a table to a string
function table_to_string(table)
  local string = "{\n"

  for key, value in pairs(table) do
    string = string .. "[" .. key .. "] = " .. to_string(value) .. "\n"
  end

  string = string .. "}"

  return string
end

-- Function to convert a string to a table
function string_to_table(string)
  local table = {}

  for line in string:gmatch("[%a] = %a") do
    local key, value = line:match("[%a] = (%a)")
    table[key] = value
  end

  return table
end

```

The code provided is a collection of various useful functions in LUA, covering a wide range of tasks. It includes functions for number manipulation, prime number checking, greatest common divisor and least common multiple calculation, random number generation, array manipulation, sorting, searching, and string conversion. These functions can be used in a variety of programs and scripts to perform common tasks efficiently.

Here's a brief explanation of each function:

1. factorial(n): Calculates the factorial of a given number n.

2. is_prime(n): Checks if a given number n is prime.

3. gcd(a, b): Finds the greatest common divisor of two numbers a and b.

4. lcm(a, b): Finds the least common multiple of two numbers a and b.

5. random_range(min, max): Generates a random number between a given minimum and maximum value.

6. random_choice(choices): Selects a random element from a given array of choices.

7. shuffle(array): Shuffles the elements of a given array in a random order.

8. sort(array): Sorts the elements of a given array in ascending order.

9. reverse(array): Reverses the order of elements in a given array.

10. find_index(array, element): Finds the index of a given element in an array.

11. remove_element(array, element): Removes a given element from an array.

12. insert_element(array, element, index): Inserts a given element into an array at a specified index.

13. to_string(number): Converts a given number to a string.

14. to_number(string): Converts a given string to a number.

15. table_to_string(table): Converts a given table to a string representation.

16. string_to_table(string): Converts a given string representation back to a table.

These functions are widely used in programming tasks and can be easily integrated into larger programs or scripts to perform specific tasks or solve problems.