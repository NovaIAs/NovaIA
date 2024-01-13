```lua
-- Define a function to calculate the factorial of a number
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Define a function to check if a number is prime
function is_prime(n)
  if n <= 1 then
    return false
  end
  for i = 2, n-1 do
    if n % i == 0 then
      return false
    end
  end
  return true
end

-- Define a function to find the largest prime factor of a number
function largest_prime_factor(n)
  local i = 2
  while n > 1 do
    if n % i == 0 and is_prime(i) then
      n = n / i
    else
      i = i + 1
    end
  end
  return i
end

-- Define a function to find all the prime factors of a number
function prime_factors(n)
  local factors = {}
  local i = 2
  while n > 1 do
    if n % i == 0 and is_prime(i) then
      table.insert(factors, i)
      n = n / i
    else
      i = i + 1
    end
  end
  return factors
end

-- Define a function to find the greatest common divisor of two numbers
function gcd(a, b)
  if b == 0 then
    return a
  else
    return gcd(b, a % b)
  end
end

-- Define a function to find the least common multiple of two numbers
function lcm(a, b)
  return (a * b) / gcd(a, b)
end

-- Define a function to find the sum of the digits of a number
function sum_of_digits(n)
  local sum = 0
  while n > 0 do
    sum = sum + n % 10
    n = n / 10
  end
  return sum
end

-- Define a function to reverse a string
function reverse_string(s)
  local reversed = ""
  for i = #s, 1, -1 do
    reversed = reversed .. s:sub(i, i)
  end
  return reversed
end

-- Define a function to check if a string is a palindrome
function is_palindrome(s)
  return s == reverse_string(s)
end

-- Define a function to find the longest palindromic substring of a string
function longest_palindrome(s)
  local longest = ""
  local start = 1
  local end = 1
  for i = 1, #s do
    for j = i+1, #s do
      local substring = s:sub(i, j)
      if is_palindrome(substring) and #substring > #longest then
        longest = substring
        start = i
        end = j
      end
    end
  end
  return longest
end

-- Define a function to find all the permutations of a string
function permutations(s)
  local result = {}
  if #s == 1 then
    return {s}
  end
  for i = 1, #s do
    local c = s:sub(i, i)
    local remaining = s:sub(1, i-1) .. s:sub(i+1)
    local sub_permutations = permutations(remaining)
    for j = 1, #sub_permutations do
      table.insert(result, c .. sub_permutations[j])
    end
  end
  return result
end

-- Define a function to find all the combinations of a set of elements
function combinations(set, k)
  local result = {}
  if k == 1 then
    for i = 1, #set do
      table.insert(result, {set[i]})
    end
    return result
  end
  for i = 1, #set-k+1 do
    local c = set[i]
    local remaining = {}
    for j = i+1, #set do
      table.insert(remaining, set[j])
    end
    local sub_combinations = combinations(remaining, k-1)
    for j = 1, #sub_combinations do
      table.insert(result, {c} .. sub_combinations[j])
    end
  end
  return result
end

-- Define a function to find all the subsets of a set of elements
function subsets(set)
  local result = {}
  for i = 0, #set do
    local sub_result = combinations(set, i)
    for j = 1, #sub_result do
      table.insert(result, sub_result[j])
    end
  end
  return result
end

-- Define a function to find the power set of a set of elements
function power_set(set)
  return subsets(set)
end

-- Define a function to find the Cartesian product of two sets
function cartesian_product(set1, set2)
  local result = {}
  for i = 1, #set1 do
    for j = 1, #set2 do
      table.insert(result, {set1[i], set2[j]})
    end
  end
  return result
end
```

This code defines a number of useful mathematical and string manipulation functions in Lua. The functions include factorial, primality testing, finding the largest prime factor, finding all prime factors, finding the greatest common divisor, finding the least common multiple, summing the digits of a number, reversing a string, checking if a string is a palindrome, finding the longest palindromic substring, finding all permutations of a string, finding all combinations of a set of elements, finding all subsets of a set of elements, finding the power set of a set of elements, and finding the Cartesian product of two sets. These functions can be used for a wide variety of programming tasks, including solving mathematical problems, manipulating strings, and generating data structures.