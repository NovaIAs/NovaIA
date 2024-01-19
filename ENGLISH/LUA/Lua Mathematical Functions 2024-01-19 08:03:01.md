```lua
-- This function returns the factorial of a number.
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- This function generates a random number between 1 and 100.
function random_number()
  return math.random(1, 100)
end

-- This function takes a table of numbers and returns the sum of the numbers in the table.
function sum_table(t)
  local sum = 0
  for i,v in pairs(t) do
    sum = sum + v
  end
  return sum
end

-- This function takes two tables of numbers and returns a new table that contains the union of the two tables.
function union(t1, t2)
  local t3 = {}
  for i,v in pairs(t1) do
    t3[v] = true
  end
  for i,v in pairs(t2) do
    t3[v] = true
  end
  return t3
end

-- This function takes two tables of numbers and returns a new table that contains the intersection of the two tables.
function intersection(t1, t2)
  local t3 = {}
  for i,v in pairs(t1) do
    if t2[v] then
      t3[v] = true
    end
  end
  return t3
end

-- This function takes a table of numbers and returns a new table that contains the difference of the two tables.
function difference(t1, t2)
  local t3 = {}
  for i,v in pairs(t1) do
    if not t2[v] then
      t3[v] = true
    end
  end
  return t3
end

-- This function takes a table of numbers and returns a new table that contains the symmetric difference of the two tables.
function symmetric_difference(t1, t2)
  local t3 = {}
  for i,v in pairs(t1) do
    if not t2[v] then
      t3[v] = true
    end
  end
  for i,v in pairs(t2) do
    if not t1[v] then
      t3[v] = true
    end
  end
  return t3
end
```

This code contains a variety of functions that can be used to perform various mathematical operations. The functions include:

* `factorial`: Calculates the factorial of a number.
* `random_number`: Generates a random number between 1 and 100.
* `sum_table`: Calculates the sum of the numbers in a table.
* `union`: Takes two tables of numbers and returns a new table that contains the union of the two tables.
* `intersection`: Takes two tables of numbers and returns a new table that contains the intersection of the two tables.
* `difference`: Takes two tables of numbers and returns a new table that contains the difference of the two tables.
* `symmetric_difference`: Takes two tables of numbers and returns a new table that contains the symmetric difference of the two tables.