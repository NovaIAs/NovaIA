```lua
-- Create a function to calculate the factorial of a number
function factorial(n)
   if n == 0 then
      return 1
   else
      return n * factorial(n-1)
   end
end

-- Create a function to calculate the nth Fibonacci number
function fibonacci(n)
   if n == 0 or n == 1 then
      return n
   else
      return fibonacci(n-1) + fibonacci(n-2)
   end
end

-- Create a function to check if a number is prime
function is_prime(n)
   if n < 2 then
      return false
   end
   for i = 2, n/2 do
      if n % i == 0 then
         return false
      end
   end
   return true
end

-- Create a function to generate a random number between two numbers
function random_number(min, max)
   return math.random(min, max)
end

-- Create a function to sort an array of numbers in ascending order
function sort_array(array)
   for i = 1, #array do
      for j = i+1, #array do
         if array[i] > array[j] then
            temp = array[i]
            array[i] = array[j]
            array[j] = temp
         end
      end
   end
   return array
end

-- Create a function to search for a value in an array using binary search
function binary_search(array, value)
   local low = 1
   local high = #array

   while low <= high do
      local mid = math.floor((low + high) / 2)
      if array[mid] == value then
         return mid
      elseif array[mid] < value then
         low = mid + 1
      else
         high = mid - 1
      end
   end

   return -1
end

-- Create a function to create a linked list
function create_linked_list(values)
   local head = nil
   local tail = nil

   for i = 1, #values do
      local new_node = {value = values[i], next = nil}
      if head == nil then
         head = new_node
         tail = new_node
      else
         tail.next = new_node
         tail = new_node
      end
   end

   return head
end

-- Create a function to print the values of a linked list
function print_linked_list(head)
   local current = head
   while current ~= nil do
      print(current.value)
      current = current.next
   end
end

-- Create a function to reverse a linked list
function reverse_linked_list(head)
   local previous = nil
   local current = head
   local next = nil

   while current ~= nil do
      next = current.next
      current.next = previous
      previous = current
      current = next
   end

   return previous
end

-- Create a function to find the middle node of a linked list
function find_middle_node(head)
   local slow = head
   local fast = head

   while fast ~= nil and fast.next ~= nil do
      slow = slow.next
      fast = fast.next.next
   end

   return slow
end

-- Create a function to check if a linked list is a palindrome
function is_palindrome(head)
   local reversed_head = reverse_linked_list(head)
   local current1 = head
   local current2 = reversed_head

   while current1 ~= nil and current2 ~= nil do
      if current1.value ~= current2.value then
         return false
      end
      current1 = current1.next
      current2 = current2.next
   end

   return true
end

-- Create a function to merge two sorted linked lists
function merge_sorted_linked_lists(head1, head2)
   local dummy_head = {value = 0, next = nil}
   local current = dummy_head

   while head1 ~= nil and head2 ~= nil do
      if head1.value < head2.value then
         current.next = head1
         head1 = head1.next
      else
         current.next = head2
         head2 = head2.next
      end
      current = current.next
   end

   if head1 ~= nil then
      current.next = head1
   elseif head2 ~= nil then
      current.next = head2
   end

   return dummy_head.next
end

-- Create a function to sort a linked list using merge sort
function merge_sort_linked_list(head)
   if head == nil or head.next == nil then
      return head
   end

   local middle_node = find_middle_node(head)
   local next_of_middle_node = middle_node.next
   middle_node.next = nil

   local left_half = merge_sort_linked_list(head)
   local right_half = merge_sort_linked_list(next_of_middle_node)

   return merge_sorted_linked_lists(left_half, right_half)
end

-- Create a function to find the kth node from the end of a linked list
function find_kth_node_from_end(head, k)
   local fast = head
   local slow = head

   for i = 1, k do
      fast = fast.next
   end

   while fast ~= nil do
      fast = fast.next
      slow = slow.next
   end

   return slow
end

-- Explanation of the Code:

-- 1. Factorial Function:
```lua
function factorial(n)
   if n == 0 then
      return 1
   else
      return n * factorial(n-1)
   end
end
```
This function calculates the factorial of a number using recursion.

-- 2. Fibonacci Function:
```lua
function fibonacci(n)
   if n == 0 or n == 1 then
      return n
   else
      return fibonacci(n-1) + fibonacci(n-2)
   end
end
```
This function calculates the nth Fibonacci number using recursion.

-- 3. Is Prime Function:
```lua
function is_prime(n)
   if n < 2 then
      return false
   end
   for i = 2, n/2 do
      if n % i == 0 then
         return false
      end
   end
   return true
end
```
This function checks if a number is prime.

-- 4. Random Number Function:
```lua
function random_number(min, max)
   return math.random(min, max)
end
```
This function generates a random number between two numbers.

-- 5. Sort Array Function:
```lua
function sort_array(array)
   for i = 1, #array do
      for j = i+1, #array do
         if array[i] > array[j] then
            temp = array[i]
            array[i] = array[j]
            array[j] = temp
         end
      end
   end
   return array
end
```
This function sorts an array of numbers in ascending order using the bubble sort algorithm.

-- 6. Binary Search Function:
```lua
function binary_search(array, value)
   local low = 1
   local high = #array

   while low <= high do
      local mid = math.floor((low + high) / 2)
      if array[mid] == value then
         return mid
      elseif array[mid] < value then
         low = mid + 1
      else
         high = mid - 1
      end
   end

   return -1
end
```
This function searches for a value in an array using binary search.

-- 7. Create Linked List Function:
```lua
function create_linked_list(values)
   local head = nil
   local tail = nil

   for i = 1, #values do
      local new_node = {value = values[i], next = nil}
      if head == nil then
         head = new_node
         tail = new_node
      else
         tail.next = new_node
         tail = new_node
      end
   end

   return head
end
```
This function creates a linked list from an array of values.

-- 8. Print Linked List Function:
```lua
function print_linked_list(head)
   local current = head
   while current ~= nil do
      print(current.value)
      current = current.next
   end
end
```
This function prints the values of a linked list.

-- 9. Reverse Linked List Function:
```lua
function reverse_linked_list(head)
   local previous = nil
   local current = head