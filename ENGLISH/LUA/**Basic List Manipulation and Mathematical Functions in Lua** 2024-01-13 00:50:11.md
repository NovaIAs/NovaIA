```lua
-- Create a function to calculate the factorial of a number
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Create a function to calculate the sum of a list of numbers
function sum(list)
  if #list == 0 then
    return 0
  else
    return list[1] + sum(list:sub(2))
  end
end

-- Create a function to calculate the product of a list of numbers
function product(list)
  if #list == 0 then
    return 1
  else
    return list[1] * product(list:sub(2))
  end
end

-- Create a function to calculate the average of a list of numbers
function average(list)
  if #list == 0 then
    return 0
  else
    return sum(list) / #list
  end
end

-- Create a function to find the maximum value in a list of numbers
function max(list)
  if #list == 0 then
    return nil
  else
    local max_value = list[1]
    for i=2,#list do
      if list[i] > max_value then
        max_value = list[i]
      end
    end
    return max_value
  end
end

-- Create a function to find the minimum value in a list of numbers
function min(list)
  if #list == 0 then
    return nil
  else
    local min_value = list[1]
    for i=2,#list do
      if list[i] < min_value then
        min_value = list[i]
      end
    end
    return min_value
  end
end

-- Create a function to sort a list of numbers in ascending order
function sort(list)
  if #list == 0 then
    return {}
  else
    local pivot = list[1]
    local less = {}
    local greater = {}
    for i=2,#list do
      if list[i] < pivot then
        table.insert(less, list[i])
      else
        table.insert(greater, list[i])
      end
    end
    return table.concat({sort(less), {pivot}, sort(greater)})
  end
end

-- Create a function to reverse a list
function reverse(list)
  if #list == 0 then
    return {}
  else
    return {list[#list]} .. reverse(list:sub(1,#list-1))
  end
end

-- Create a function to find the index of a value in a list
function find(list, value)
  for i=1,#list do
    if list[i] == value then
      return i
    end
  end
  return nil
end

-- Create a function to remove a value from a list
function remove(list, value)
  local index = find(list, value)
  if index then
    table.remove(list, index)
  end
  return list
end

-- Create a function to insert a value into a list at a specific index
function insert(list, value, index)
  if index == 1 then
    table.insert(list, 1, value)
  elseif index > #list then
    table.insert(list, #list+1, value)
  else
    table.insert(list, index, value)
  end
  return list
end

-- Create a function to merge two lists
function merge(list1, list2)
  local merged_list = {}
  while #list1 > 0 and #list2 > 0 do
    if list1[1] < list2[1] then
      table.insert(merged_list, list1[1])
      table.remove(list1, 1)
    else
      table.insert(merged_list, list2[1])
      table.remove(list2, 1)
    end
  end
  while #list1 > 0 do
    table.insert(merged_list, list1[1])
    table.remove(list1, 1)
  end
  while #list2 > 0 do
    table.insert(merged_list, list2[1])
    table.remove(list2, 1)
  end
  return merged_list
end

-- Create a function to split a list into two lists
function split(list, index)
  local list1 = {}
  local list2 = {}
  for i=1,index do
    table.insert(list1, list[i])
  end
  for i=index+1,#list do
    table.insert(list2, list[i])
  end
  return list1, list2
end

-- Create a function to rotate a list by a specified number of positions
function rotate(list, n)
  local new_list = {}
  for i=1,#list do
    local index = (i + n - 1) % #list + 1
    table.insert(new_list, list[index])
  end
  return new_list
end

-- Create a function to find the longest common subsequence between two strings
function lcs(str1, str2)
  local m = #str1
  local n = #str2
  local lcs_matrix = {}
  for i=0,m do
    lcs_matrix[i] = {}
    for j=0,n do
      lcs_matrix[i][j] = 0
    end
  end
  for i=1,m do
    for j=1,n do
      if str1[i] == str2[j] then
        lcs_matrix[i][j] = lcs_matrix[i-1][j-1] + 1
      else
        lcs_matrix[i][j] = math.max(lcs_matrix[i-1][j], lcs_matrix[i][j-1])
      end
    end
  end
  local lcs_length = lcs_matrix[m][n]
  local lcs_string = ""
  local i = m
  local j = n
  while lcs_length > 0 do
    if str1[i] == str2[j] then
      lcs_string = str1[i] .. lcs_string
      lcs_length = lcs_length - 1
      i = i - 1
      j = j - 1
    else
      if lcs_matrix[i-1][j] > lcs_matrix[i][j-1] then
        i = i - 1
      else
        j = j - 1
      end
    end
  end
  return lcs_string
end

-- Create a function to find the shortest common supersequence between two strings
function scs(str1, str2)
  local m = #str1
  local n = #str2
  local scs_matrix = {}
  for i=0,m do
    scs_matrix[i] = {}
    for j=0,n do
      scs_matrix[i][j] = 0
    end
  end
  for i=