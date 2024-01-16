```lua
local function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

local function fibonacci(n)
  if n == 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fibonacci(n-1) + fibonacci(n-2)
  end
end

local function gcd(a, b)
  if b == 0 then
    return a
  else
    return gcd(b, a % b)
  end
end

local function lcm(a, b)
  return (a * b) / gcd(a, b)
end

local function is_prime(n)
  if n <= 1 then
    return false
  elseif n == 2 then
    return true
  else
    for i = 2, math.floor(math.sqrt(n)) do
      if n % i == 0 then
        return false
      end
    end
    return true
  end
end

local function find_prime_factors(n)
  local factors = {}
  for i = 2, math.floor(math.sqrt(n)) do
    while n % i == 0 do
      table.insert(factors, i)
      n = n / i
    end
  end
  if n > 1 then
    table.insert(factors, n)
  end
  return factors
end

local function merge_sort(array)
  if #array <= 1 then
    return array
  end

  local middle = math.floor(#array / 2)
  local left = merge_sort(array:sub(1, middle))
  local right = merge_sort(array:sub(middle+1, #array))

  local merged = {}
  local left_index = 1
  local right_index = 1

  while left_index <= #left and right_index <= #right do
    if left[left_index] < right[right_index] then
      table.insert(merged, left[left_index])
      left_index = left_index + 1
    else
      table.insert(merged, right[right_index])
      right_index = right_index + 1
    end
  end

  while left_index <= #left do
    table.insert(merged, left[left_index])
    left_index = left_index + 1
  end

  while right_index <= #right do
    table.insert(merged, right[right_index])
    right_index = right_index + 1
  end

  return merged
end

local function quick_sort(array)
  if #array <= 1 then
    return array
  end

  local pivot = array[#array]
  local left = {}
  local right = {}

  for i = 1, #array-1 do
    if array[i] < pivot then
      table.insert(left, array[i])
    else
      table.insert(right, array[i])
    end
  end

  local sorted_left = quick_sort(left)
  local sorted_right = quick_sort(right)

  return table.concat({sorted_left, {pivot}, sorted_right})
end

local function insertion_sort(array)
  for i = 2, #array do
    local key = array[i]
    local j = i - 1

    while j >= 1 and array[j] > key do
      array[j+1] = array[j]
      j = j - 1
    end

    array[j+1] = key
  end

  return array
end

local function selection_sort(array)
  for i = 1, #array do
    local min_index = i

    for j = i+1, #array do
      if array[j] < array[min_index] then
        min_index = j
      end
    end

    local temp = array[i]
    array[i] = array[min_index]
    array[min_index] = temp
  end

  return array
end

local function bubble_sort(array)
  for i = 1, #array do
    for j = 2, #array do
      if array[j] < array[j-1] then
        local temp = array[j]
        array[j] = array[j-1]
        array[j-1] = temp
      end
    end
  end

  return array
end

local function radix_sort(array)
  local max_value = math.max(array)
  local exp = 1

  while max_value / exp > 0 do
    local buckets = {}
    for i = 0, 9 do
      buckets[i] = {}
    end

    for i = 1, #array do
      local digit = math.floor((array[i] / exp) % 10)
      table.insert(buckets[digit], array[i])
    end

    local index = 1
    for i = 0, 9 do
      for j = 1, #buckets[i] do
        array[index] = buckets[i][j]
        index = index + 1
      end
    end

    exp = exp * 10
  end

  return array
end

local function counting_sort(array)
  local max_value = math.max(array)
  local counts = {}

  for i = 0, max_value do
    counts[i] = 0
  end

  for i = 1, #array do
    counts[array[i]] = counts[array[i]] + 1
  end

  local index = 1
  for i = 0, max_value do
    while counts[i] > 0 do
      array[index] = i
      index = index + 1
      counts[i] = counts[i] - 1
    end
  end

  return array
end

local function bucket_sort(array)
  local max_value = math.max(array)
  local min_value = math.min(array)
  local bucket_size = math.ceil((max_value - min_value) / #array)
  local buckets = {}

  for i = 1, #array do
    local bucket_index = math.floor((array[i] - min_value) / bucket_size)
    if buckets[bucket_index] == nil then
      buckets[bucket_index] = {}
    end
    table.insert(buckets[bucket_index], array[i])
  end

  local index = 1
  for i = 1, #buckets do
    for j = 1, #buckets[i] do
      array[index] = buckets[i][j]
      index = index + 1
    end
  end

  return array
end

local function heap_sort(array)
  local heap_size = #array

  for i = math.floor(heap_size / 2), 1, -1 do
    heapify(array, heap_size, i)
  end

  for i = heap_size, 1, -1 do
    local temp = array[1]
    array[1] = array[i]
    array[i] = temp

    heap_size = heap_size - 1
    heapify(array, heap_size, 1)
  end

  return array

  function heapify(array, heap_size, i)
    local largest = i
    local left = 2 * i
    local right = 2 * i + 1

    if left <= heap_size and array[left] > array[largest] then
      largest = left
    end

    if right <= heap_size and array[right] > array[largest] then
      largest = right
    end

    if largest ~= i then
      local temp = array[i]
      array[i] = array[largest]
      array[largest] = temp

      heapify(array, heap_size, largest)
    end
  end
end

local function topological_sort(graph)
  local visited = {}
  local stack = {}

  for node in graph do
    if not visited[node] then
      topological_sort_dfs(graph, node, visited, stack)
    end
  end

  return stack

  function topological_sort_dfs(graph, node, visited, stack)
    visited[node] = true

    for neighbor in graph[node] do
      if not visited[neighbor] then
        topological_sort_dfs(graph, neighbor, visited, stack)
      end
    end

    table.insert