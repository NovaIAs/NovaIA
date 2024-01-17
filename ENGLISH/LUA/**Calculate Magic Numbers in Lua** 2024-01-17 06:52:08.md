```lua
-- This Lua code defines a function called "calculateMagicNumbers"
-- that takes an integer "n" as input and returns a table containing
-- two magic numbers calculated based on the input.

function calculateMagicNumbers(n)
  -- Check if the input is a valid integer.
  if type(n) ~= "number" or n <= 0 then
    error("Invalid input: n must be a positive integer.")
  end

  -- Initialize two variables to store the magic numbers.
  local magicNumber1 = 0
  local magicNumber2 = 0

  -- Calculate the first magic number by summing the digits of n.
  for digit in string.gmatch(tostring(n), "(%d)") do
    magicNumber1 = magicNumber1 + tonumber(digit)
  end

  -- Calculate the second magic number by finding the sum of the squares
  -- of the digits of n.
  for digit in string.gmatch(tostring(n), "(%d)") do
    magicNumber2 = magicNumber2 + tonumber(digit) ^ 2
  end

  -- Return the magic numbers in a table.
  return {
    magicNumber1 = magicNumber1,
    magicNumber2 = magicNumber2
  }
end

-- Example usage of the "calculateMagicNumbers" function.
local inputNumber = 12345
local result = calculateMagicNumbers(inputNumber)

-- Print the magic numbers to the console.
print("Magic Number 1:", result.magicNumber1)
print("Magic Number 2:", result.magicNumber2)
```

**Explanation:**

1. **Function Definition**: The code defines a function named "calculateMagicNumbers" that takes one argument, "n," which is expected to be a positive integer.

2. **Input Validation**: The function first checks if the input "n" is a valid integer and greater than 0. If not, it throws an error.

3. **Magic Number Calculations**:
   - **Magic Number 1**: To calculate the first magic number, the code loops through the individual digits of "n" using the string.gmatch() function. It adds each digit to the "magicNumber1" variable, effectively summing the digits of "n."
   - **Magic Number 2**: For the second magic number, the code again loops through the digits of "n," but this time it squares each digit, adds it to the "magicNumber2" variable, and sums the squared digits.

4. **Return Value**: The function returns a table containing two fields: "magicNumber1" and "magicNumber2," which store the calculated magic numbers.

5. **Example Usage**: In the example usage section, the code calls the "calculateMagicNumbers" function with the input number "12345" and assigns the result to the "result" variable.

6. **Printing the Results**: Finally, the code prints the magic numbers to the console using the print() function.

This code demonstrates a complex and differentiated task in Lua by calculating two distinct magic numbers based on the input integer "n" and returning them in a table. It involves string manipulation, looping, and mathematical operations, making it a comprehensive and unique code snippet.