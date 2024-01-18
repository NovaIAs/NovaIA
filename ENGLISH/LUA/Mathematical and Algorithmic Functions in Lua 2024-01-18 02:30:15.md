```lua
-- Create a function to find the factorial of a number
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end

-- Create a function to generate a table of prime numbers
function prime_sieve(n)
  local primes = {}
  local sieve = {}
  for i = 2, n do
    sieve[i] = true
  end
  for p = 2, math.sqrt(n) do
    if sieve[p] then
      for i = p * p, n, p do
        sieve[i] = false
      end
    end
  end
  for i = 2, n do
    if sieve[i] then
      primes[#primes + 1] = i
    end
  end
  return primes
end

-- Create a function to find the largest prime factor of a number
function largest_prime_factor(n)
  local i = 2
  while i <= n do
    if n % i == 0 then
      n = n / i
    else
      i = i + 1
    end
  end
  return i
end

-- Create a function to find the smallest positive integer that is evenly divisible by all of the numbers from 1 to n
function smallest_multiple(n)
  local primes = prime_sieve(n)
  local exponents = {}
  for i = 1, n do
    local prime = primes[i]
    while n % prime == 0 do
      exponents[prime] = exponents[prime] + 1
      n = n / prime
    end
  end
  local smallest_multiple = 1
  for prime, exponent in pairs(exponents) do
    smallest_multiple = smallest_multiple * math.pow(prime, exponent)
  end
  return smallest_multiple
end

-- Create a function to find the sum of the digits of a number
function sum_of_digits(n)
  local sum = 0
  while n > 0 do
    sum = sum + n % 10
    n = math.floor(n / 10)
  end
  return sum
end

-- Create a function to find the number of ways to make change for a given amount of money using a given set of coins
function num_ways_to_change(amount, coins)
  local dp = {}
  dp[0] = 1
  for i = 1, amount do
    dp[i] = 0
  end
  for j = 1, #coins do
    for i = coins[j], amount do
      dp[i] = dp[i] + dp[i - coins[j]]
    end
  end
  return dp[amount]
end

-- Create a function to find the longest increasing subsequence of a given array
function longest_increasing_subsequence(arr)
  local dp = {}
  for i = 1, #arr do
    dp[i] = 1
  end
  local longest_subsequence = 1
  for i = 2, #arr do
    for j = 1, i - 1 do
      if arr[i] > arr[j] and dp[i] < dp[j] + 1 then
        dp[i] = dp[j] + 1
        if dp[i] > longest_subsequence then
          longest_subsequence = dp[i]
        end
      end
    end
  end
  return longest_subsequence
end

-- Create a function to find the shortest path between two nodes in a graph
function shortest_path(graph, source, destination)
  local distance = {}
  local visited = {}
  for node in graph do
    distance[node] = math.huge
    visited[node] = false
  end
  distance[source] = 0
  local queue = {source}
  while #queue > 0 do
    local current = queue[1]
    table.remove(queue, 1)
    visited[current] = true
    for neighbor, weight in pairs(graph[current]) do
      if not visited[neighbor] and distance[current] + weight < distance[neighbor] then
        distance[neighbor] = distance[current] + weight
        queue[#queue + 1] = neighbor
      end
    end
  end
  return distance[destination]
end
```

This code contains a collection of complex and differentiated functions, covering a wide range of mathematical and algorithmic problems. Let's break down the code section by section:

1. **Factorial Function**:
   - Calculates the factorial of a given number using recursion.

2. **Prime Sieve Function**:
   - Generates a table of prime numbers up to a given limit using the Sieve of Eratosthenes algorithm.

3. **Largest Prime Factor Function**:
   - Finds the largest prime factor of a given number using a simple loop.

4. **Smallest Multiple Function**:
   - Determines the smallest positive integer that is evenly divisible by all numbers from 1 to a given limit.

5. **Sum of Digits Function**:
   - Computes the sum of the digits of a given number using a while loop.

6. **Number of Ways to Change Function**:
   - Calculates the number of ways to make change for a given amount of money using a set of coins using dynamic programming.

7. **Longest Increasing Subsequence Function**:
   - Finds the length of the longest increasing subsequence in a given array using dynamic programming.

8. **Shortest Path Function**:
   - Determines the shortest path between two nodes in a weighted graph using the Breadth-First Search algorithm.

This code demonstrates a variety of problem-solving techniques, including recursion, dynamic programming, and graph traversal. It showcases the versatility and expressiveness of the Lua programming language.