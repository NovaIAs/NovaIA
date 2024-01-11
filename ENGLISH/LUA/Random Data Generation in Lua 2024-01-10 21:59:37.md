```lua
-- Create a function to generate a random number between two values
function generateRandomNumber(min, max)
  -- Check if the min and max values are valid
  if min > max then
    error("Minimum value cannot be greater than maximum value.")
  end
  -- Generate a random number between min and max
  return math.random(min, max)
end

-- Create a function to generate a random string of a specified length
function generateRandomString(length)
  -- Check if the length is valid
  if length <= 0 then
    error("Length must be a positive integer.")
  end
  -- Generate a random string of the specified length
  local result = ""
  for i = 1, length do
    -- Generate a random character between 'a' and 'z' or 'A' and 'Z'
    local charCode = generateRandomNumber(97, 122)
    if generateRandomNumber(1, 2) == 1 then
      charCode = charCode - 32
    end
    -- Append the character to the result string
    result = result .. string.char(charCode)
  end
  return result
end

-- Create a function to generate a random list of integers of a specified length
function generateRandomList(length, min, max)
  -- Check if the length, min, and max values are valid
  if length <= 0 then
    error("Length must be a positive integer.")
  end
  if min > max then
    error("Minimum value cannot be greater than maximum value.")
  end
  -- Generate a random list of integers of the specified length
  local result = {}
  for i = 1, length do
    -- Generate a random integer between min and max
    result[i] = generateRandomNumber(min, max)
  end
  return result
end

-- Create a function to generate a random dictionary of key-value pairs of a specified length
function generateRandomDictionary(length, minKeyLength, maxKeyLength, minValueLength, maxValueLength)
  -- Check if the length, minKeyLength, maxKeyLength, minValueLength, and maxValueLength values are valid
  if length <= 0 then
    error("Length must be a positive integer.")
  end
  if minKeyLength > maxKeyLength then
    error("Minimum key length cannot be greater than maximum key length.")
  end
  if minValueLength > maxValueLength then
    error("Minimum value length cannot be greater than maximum value length.")
  end
  -- Generate a random dictionary of key-value pairs of the specified length
  local result = {}
  for i = 1, length do
    -- Generate a random key of the specified length
    local key = generateRandomString(generateRandomNumber(minKeyLength, maxKeyLength))
    -- Generate a random value of the specified length
    local value = generateRandomString(generateRandomNumber(minValueLength, maxValueLength))
    -- Add the key-value pair to the dictionary
    result[key] = value
  end
  return result
end

-- Create a function to print the results of the random data generation
function printResults(results)
  -- Print the results to the console
  print("Random Number: " .. results.randomNumber)
  print("Random String: " .. results.randomString)
  print("Random List: " .. table.concat(results.randomList, ", "))
  print("Random Dictionary: ")
  for key, value in pairs(results.randomDictionary) do
    print("  " .. key .. ": " .. value)
  end
end

-- Generate the random data
local results = {
  randomNumber = generateRandomNumber(1, 100),
  randomString = generateRandomString(10),
  randomList = generateRandomList(5, 1, 10),
  randomDictionary = generateRandomDictionary(3, 5, 10, 5, 10)
}

-- Print the results to the console
printResults(results)
```

Explanation:

1. We define several functions to generate random data:

   - `generateRandomNumber(min, max)`: Generates a random number between two values.
   - `generateRandomString(length)`: Generates a random string of a specified length.
   - `generateRandomList(length, min, max)`: Generates a random list of integers of a specified length.
   - `generateRandomDictionary(length, minKeyLength, maxKeyLength, minValueLength, maxValueLength)`: Generates a random dictionary of key-value pairs of a specified length.

2. We create a function called `printResults` to print the results of the random data generation to the console.

3. We generate the random data using the functions we defined and store the results in a table called `results`.

4. Finally, we call the `printResults` function to print the results to the console.

This code demonstrates the generation of various types of random data, including numbers, strings, lists, and dictionaries. It also showcases the use of functions and tables in Lua.