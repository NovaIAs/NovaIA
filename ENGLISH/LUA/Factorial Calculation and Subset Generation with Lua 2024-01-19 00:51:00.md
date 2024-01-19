```LUA
-- This Lua code implements a simple function to calculate the factorial of a number.
-- Factorial is the product of all positive integers less than or equal to the given number.
-- The function takes a non-negative integer as its argument and returns its factorial.

-- Define the factorial function.
function factorial(n)
    -- If the given number is less than 0, we cannot calculate its factorial.
    if n < 0 then
        error("Factorial is not defined for negative numbers.")
    end

    -- If the given number is 0 or 1, its factorial is 1.
    if n == 0 or n == 1 then
        return 1
    end

    -- Initialize the factorial to 1.
    local factorial = 1

    -- Iterate over the numbers from 2 to the given number and multiply them together.
    for i = 2, n do
        factorial = factorial * i
    end

    -- Return the factorial.
    return factorial
end

-- Print the factorials of the numbers from 1 to 10.
for i = 1, 10 do
    print(i .. "! = " .. factorial(i))
end
```

This Lua code accomplishes a more complex task by defining a function to generate all possible subsets of a given set. A subset is a collection of some elements from a set, without regard to the order of those elements. The function takes a set as its argument and returns a table containing all subsets of the set.

```LUA
-- This Lua code defines a function to generate all possible subsets of a given set.

-- Define the function to generate subsets.
function subsets(set)
    -- If the set is empty, there is only one subset: the empty set.
    if set == {} then
        return {{} }
    end

    -- Choose the first element of the set.
    local first = set[1]

    -- Generate subsets of the set without the first element.
    local rest_subsets = subsets(table.remove(set, 1))

    -- Create new subsets by adding the first element to each subset in rest_subsets.
    local new_subsets = {}
    for i, subset in ipairs(rest_subsets) do
        table.insert(new_subsets, {first, ...subset})
    end

    -- Combine the subsets of the set with and without the first element.
    return {rest_subsets, new_subsets}
end

-- Get all subsets of the set {1, 2, 3}.
local set = {1, 2, 3}
local all_subsets = subsets(set)

-- Print all subsets.
for i, subset in ipairs(all_subsets) do
    print("Subset " .. i .. ": " .. table.concat(subset, ", "))
end
```