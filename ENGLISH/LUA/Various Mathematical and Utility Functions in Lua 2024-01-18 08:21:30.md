```lua
function fib(n)
  if n <= 1 then
    return n
  else
    return fib(n - 1) + fib(n - 2)
  end
end

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

function factorial(n)
  if n <= 1 then
    return 1
  else
    return n * factorial(n - 1)
  end
end

function gcd(a, b)
  if b == 0 then
    return a
  else
    return gcd(b, a % b)
  end
end

function lcm(a, b)
  return a * b / gcd(a, b)
end

function is_palindrome(str)
  local len = string.len(str)
  for i = 1, len / 2 do
    if str:sub(i, i) ~= str:sub(len - i + 1, len - i + 1) then
      return false
    end
  end
  return true
end

function reverse_string(str)
  local new_str = ""
  for i = string.len(str), 1, -1 do
    new_str = new_str .. str:sub(i, i)
  end
  return new_str
end

function merge_sort(array)
  if #array <= 1 then
    return array
  end
  local mid = math.floor(#array / 2)
  local left = merge_sort(array:sub(1, mid))
  local right = merge_sort(array:sub(mid + 1, #array))
  return merge(left, right)
end

function merge(left, right)
  local merged = {}
  local i = 1
  local j = 1
  while i <= #left and j <= #right do
    if left[i] <= right[j] then
      table.insert(merged, left[i])
      i = i + 1
    else
      table.insert(merged, right[j])
      j = j + 1
    end
  end
  while i <= #left do
    table.insert(merged, left[i])
    i = i + 1
  end
  while j <= #right do
    table.insert(merged, right[j])
    j = j + 1
  end
  return merged
end

function quick_sort(array)
  if #array <= 1 then
    return array
  end
  local pivot = array[1]
  local left = {}
  local right = {}
  for i = 2, #array do
    if array[i] <= pivot then
      table.insert(left, array[i])
    else
      table.insert(right, array[i])
    end
  end
  return table.concat(quick_sort(left), {pivot}, quick_sort(right))
end

function binary_search(array, target)
  local low = 1
  local high = #array
  while low <= high do
    local mid = math.floor((low + high) / 2)
    if array[mid] == target then
      return mid
    elseif array[mid] < target then
      low = mid + 1
    else
      high = mid - 1
    end
  end
  return -1
end
```

This code is a collection of various mathematical and utility functions in Lua. It includes functions for calculating Fibonacci numbers, checking if a number is prime, finding the factorial of a number, finding the greatest common divisor (GCD) and least common multiple (LCM) of two numbers, checking if a string is a palindrome, reversing a string, and sorting arrays using merge sort and quick sort. It also includes a binary search function for finding a target value in a sorted array.

Here are some explanations for the code:

* The `fib` function calculates Fibonacci numbers using a recursive algorithm.
* The `is_prime` function checks if a number is prime by checking for divisibility by all numbers up to its square root.
* The `factorial` function calculates the factorial of a number using a recursive algorithm.
* The `gcd` function finds the greatest common divisor of two numbers using the Euclidean algorithm.
* The `lcm` function finds the least common multiple of two numbers using the formula LCM(a, b) = (a * b) / GCD(a, b).
* The `is_palindrome` function checks if a string is a palindrome by comparing characters at the beginning and end of the string.
* The `reverse_string` function reverses a string by iterating over its characters in reverse order.
* The `merge_sort` function sorts an array using the merge sort algorithm.
* The `quick_sort` function sorts an array using the quick sort algorithm.
* The `binary_search` function searches for a target value in a sorted array using the binary search algorithm.

This code demonstrates the use of various algorithms and data structures in Lua. It can be used as a reference or as a starting point for implementing your own algorithms and data structures.