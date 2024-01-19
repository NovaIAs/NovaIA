```lua
function even_or_odd(n)
  if n % 2 == 0 then
    return "even"
  else
    return "odd"
  end
end

function sum_of_digits(n)
  local sum = 0
  while n > 0 do
    sum = sum + n % 10
    n = n // 10
  end
  return sum
end

function perfect_number(n)
  local sum = 0
  for i = 1, n - 1 do
    if n % i == 0 then
      sum = sum + i
    end
  end
  if sum == n then
    return true
  else
    return false
  end
end

function prime_factorization(n)
  local factors = {}
  local divisor = 2
  while n > 1 do
    if n % divisor == 0 then
      table.insert(factors, divisor)
      n = n / divisor
    else
      divisor = divisor + 1
    end
  end
  return factors
end

function fibonacci(n)
  if n == 0 or n == 1 then
    return n
  else
    return fibonacci(n - 1) + fibonacci(n - 2)
  end
end

function binary_search(arr, target)
  local low = 1
  local high = #arr
  while low <= high do
    local mid = (low + high) // 2
    if arr[mid] == target then
      return mid
    elseif arr[mid] < target then
      low = mid + 1
    else
      high = mid - 1
    end
  end
  return -1
end

function merge_sort(arr)
  if #arr <= 1 then
    return arr
  end

  local mid = #arr // 2
  local left_half = merge_sort(table.subarray(arr, 1, mid))
  local right_half = merge_sort(table.subarray(arr, mid + 1, #arr))

  return merge(left_half, right_half)
end

function merge(arr1, arr2)
  local merged = {}
  local i = 1
  local j = 1

  while i <= #arr1 and j <= #arr2 do
    if arr1[i] <= arr2[j] then
      table.insert(merged, arr1[i])
      i = i + 1
    else
      table.insert(merged, arr2[j])
      j = j + 1
    end
  end

  while i <= #arr1 do
    table.insert(merged, arr1[i])
    i = i + 1
  end

  while j <= #arr2 do
    table.insert(merged, arr2[j])
    j = j + 1
  end

  return merged
end
```

Explanation:

1. `even_or_odd()`: This function checks if a given number is even or odd.

2. `sum_of_digits()`: This function calculates the sum of the individual digits of a given number.

3. `perfect_number()`: This function checks if a given number is a perfect number (a positive integer that is equal to the sum of its proper positive divisors).

4. `prime_factorization()`: This function finds the prime factorization of a given number (a list of its prime factors).

5. `fibonacci()`: This function calculates the nth Fibonacci number using recursion.

6. `binary_search()`: This function performs a binary search on a sorted array to find the index of a given target value.

7. `merge_sort()`: This function sorts an array using the merge sort algorithm.

8. `merge()`: This function merges two sorted arrays into a single sorted array.