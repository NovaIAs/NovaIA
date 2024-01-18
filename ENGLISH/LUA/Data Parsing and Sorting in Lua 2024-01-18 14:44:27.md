```lua
-- Main function
function main()

    -- Create a table to store the data
    local data = {}

    -- Iterate over the input file
    for line in io.lines("input.txt") do

        -- Split the line into fields
        local fields = line:split(",")

        -- Add the fields to the table
        data[#data + 1] = {
            name = fields[1],
            age = tonumber(fields[2]),
            city = fields[3]
        }
    end

    -- Sort the table by age
    table.sort(data, function(a, b) return a.age < b.age end)

    -- Print the data
    for _, person in ipairs(data) do
        print(person.name, person.age, person.city)
    end

end

-- Call the main function
main()
```

Explanation:

1. The main function is defined, which is the entry point of the script.

2. A table named `data` is created to store the data read from the input file.

3. A `for` loop is used to iterate over each line in the input file, represented by the variable `line`.

4. Each line is split into fields using the `:split()` method, and the fields are stored in the `fields` variable.

5. A new table is created for each line of data, and the fields are added to the table using the `#data + 1` syntax, which ensures that each new table is added to the end of the `data` table.

6. The `table.sort()` function is used to sort the `data` table by the `age` field in ascending order.

7. Another `for` loop is used to iterate over the sorted `data` table and print the name, age, and city of each person.

This code reads data from an input file, parses the data into a table, sorts the data by age, and then prints the sorted data to the console.