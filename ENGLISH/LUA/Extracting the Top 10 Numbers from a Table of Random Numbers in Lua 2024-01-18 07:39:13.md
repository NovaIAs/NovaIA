```lua
-- Create a table of random numbers
local randomNumbers = {}
for i = 1, 100 do
  randomNumbers[i] = math.random()
end

-- Create a function to sort the table in descending order
local function sortDescending(a, b)
  return b - a
end

-- Sort the table in descending order
table.sort(randomNumbers, sortDescending)

-- Create a new table to store the top 10 numbers
local top10 = {}
for i = 1, 10 do
  top10[i] = randomNumbers[i]
end

-- Print the top 10 numbers
print("Top 10 numbers:")
for i = 1, 10 do
  print(top10[i])
end

-- Create a function to calculate the average of the top 10 numbers
local function average(numbers)
  local sum = 0
  for i = 1, #numbers do
    sum = sum + numbers[i]
  end
  return sum / #numbers
end

-- Calculate the average of the top 10 numbers
local averageTop10 = average(top10)

-- Print the average of the top 10 numbers
print("Average of the top 10 numbers:", averageTop10)

-- Create a function to find the median of the top 10 numbers
local function median(numbers)
  local sortedNumbers = {}
  for i = 1, #numbers do
    sortedNumbers[i] = numbers[i]
  end
  table.sort(sortedNumbers)
  if #sortedNumbers % 2 == 1 then
    return sortedNumbers[(#sortedNumbers + 1) / 2]
  else
    return (sortedNumbers[#sortedNumbers / 2] + sortedNumbers[#sortedNumbers / 2 + 1]) / 2
  end
end

-- Find the median of the top 10 numbers
local medianTop10 = median(top10)

-- Print the median of the top 10 numbers
print("Median of the top 10 numbers:", medianTop10)
```
This code generates a table of 100 random numbers, sorts the table in descending order, and extracts the top 10 numbers. It then calculates the average and median of the top 10 numbers and prints the results. This code is complex because it involves multiple functions, loops, and mathematical calculations, and it is not something that is commonly seen in everyday programming.