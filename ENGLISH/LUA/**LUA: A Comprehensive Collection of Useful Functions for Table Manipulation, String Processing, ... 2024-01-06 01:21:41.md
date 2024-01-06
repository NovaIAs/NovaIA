```lua
-- Create a function that takes a table and returns the sum of all its elements.

function sum_table(table)
    local sum = 0
    for i, value in ipairs(table) do
        sum = sum + value
    end
    return sum
end

-- Create a table with 10 random numbers.

local numbers = {}
for i = 1, 10 do
    numbers[i] = math.random(1, 100)
end

-- Call the sum_table function to calculate the sum of the numbers table.

local total = sum_table(numbers)

-- Print the result.

print("The sum of the numbers table is:", total)

-- Create a function that takes a string and returns a table of all the words in the string.

function split_string(string)
    local words = {}
    for word in string:gmatch("[%w]+") do
        words[#words + 1] = word
    end
    return words
end

-- Create a string with a sentence.

local sentence = "This is a sentence with multiple words."

-- Call the split_string function to split the sentence into a table of words.

local words = split_string(sentence)

-- Print the result.

print("The words in the sentence are:", words)

-- Create a function that takes a table and a value and returns a new table with the value inserted at the end of the table.

function insert_value(table, value)
    table[#table + 1] = value
    return table
end

-- Create a table with 5 random numbers.

local numbers = {}
for i = 1, 5 do
    numbers[i] = math.random(1, 100)
end

-- Call the insert_value function to insert the value 100 at the end of the numbers table.

local new_numbers = insert_value(numbers, 100)

-- Print the result.

print("The new numbers table is:", new_numbers)

-- Create a function that takes a table and a value and returns a new table with the value removed from the table.

function remove_value(table, value)
    local new_table = {}
    for i, v in ipairs(table) do
        if v ~= value then
            new_table[#new_table + 1] = v
        end
    end
    return new_table
end

-- Create a table with 5 random numbers.

local numbers = {}
for i = 1, 5 do
    numbers[i] = math.random(1, 100)
end

-- Call the remove_value function to remove the value 50 from the numbers table.

local new_numbers = remove_value(numbers, 50)

-- Print the result.

print("The new numbers table is:", new_numbers)

-- Create a function that takes a table and a key and returns the value associated with the key in the table.

function get_value(table, key)
    return table[key]
end

-- Create a table with 5 key-value pairs.

local table = {
    ["name"] = "John Doe",
    ["age"] = 30,
    ["city"] = "New York City",
    ["state"] = "New York",
    ["country"] = "United States"
}

-- Call the get_value function to get the value associated with the "name" key in the table.

local name = get_value(table, "name")

-- Print the result.

print("The name is:", name)

-- Create a function that takes a table and a key-value pair and inserts the key-value pair into the table.

function insert_key_value(table, key, value)
    table[key] = value
    return table
end

-- Create a table with 5 key-value pairs.

local table = {
    ["name"] = "John Doe",
    ["age"] = 30,
    ["city"] = "New York City",
    ["state"] = "New York",
    ["country"] = "United States"
}

-- Call the insert_key_value function to insert the key-value pair "job" with the value "Software Engineer" into the table.

local new_table = insert_key_value(table, "job", "Software Engineer")

-- Print the result.

print("The new table is:", new_table)
```

Explanation:

1. **Sum of a Table**: The `sum_table` function takes a table and returns the sum of all its elements. It uses a for loop to iterate over the table and adds each element to the sum.


2. **Split a String**: The `split_string` function takes a string and returns a table of all the words in the string. It uses the `gmatch` function to find all the words in the string and adds them to a table.


3. **Insert a Value into a Table**: The `insert_value` function takes a table and a value and returns a new table with the value inserted at the end of the table. It uses the `#table + 1` syntax to get the index of the last element in the table and then adds the value to the table.


4. **Remove a Value from a Table**: The `remove_value` function takes a table and a value and returns a new table with the value removed from the table. It uses a for loop to iterate over the table and adds each element to a new table, except for the value that is being removed.


5. **Get a Value from a Table**: The `get_value` function takes a table and a key and returns the value associated with the key in the table. It uses the `table[key]` syntax to get the value associated with the key.


6. **Insert a Key-Value Pair into a Table**: The `insert_key_value` function takes a table and a key-value pair and inserts the key-value pair into the table.