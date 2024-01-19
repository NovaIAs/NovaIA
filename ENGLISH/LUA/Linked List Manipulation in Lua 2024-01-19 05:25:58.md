```lua
-- This Lua code demonstrates the creation and manipulation of a linked list data structure,
-- along with helper functions for adding, removing, and searching for elements in the list.

-- Node structure for the linked list
local Node = {
  value = nil,
  next = nil
}

-- Head node of the linked list
local head = nil

-- Function to add a new node to the end of the linked list
function add_to_end(value)
  -- Create a new node
  local new_node = Node {
    value = value,
    next = nil
  }

  -- If the list is empty, set the head node to the new node
  if head == nil then
    head = new_node
  else
    -- Otherwise, find the last node in the list and add the new node after it
    local current_node = head
    while current_node.next ~= nil do
      current_node = current_node.next
    end
    current_node.next = new_node
  end
end

-- Function to remove a node from the linked list by value
function remove_by_value(value)
  -- If the list is empty, return nil
  if head == nil then
    return nil
  end

  -- If the head node matches the value, set the head node to the next node
  if head.value == value then
    head = head.next
  else
    -- Otherwise, find the node before the one to be removed
    local previous_node = head
    local current_node = head.next
    while current_node ~= nil and current_node.value ~= value do
      previous_node = current_node
      current_node = current_node.next
    end

    -- If the node to be removed is found, remove it from the list
    if current_node ~= nil then
      previous_node.next = current_node.next
    end
  end
end

-- Function to search for a node in the linked list by value
function search_by_value(value)
  -- If the list is empty, return nil
  if head == nil then
    return nil
  end

  -- Traverse the list and return the node if its value matches the given value
  local current_node = head
  while current_node ~= nil do
    if current_node.value == value then
      return current_node
    end
    current_node = current_node.next
  end

  -- If the node is not found, return nil
  return nil
end

-- Function to print the values of the linked list
function print_list()
  -- If the list is empty, print an empty message
  if head == nil then
    print("List is empty.")
  else
    -- Otherwise, traverse the list and print the values of each node
    local current_node = head
    while current_node ~= nil do
      print(current_node.value)
      current_node = current_node.next
    end
  end
end

-- Example usage of the linked list functions
add_to_end(10)
add_to_end(20)
add_to_end(30)
print_list() -- Output: 10 20 30

remove_by_value(20)
print_list() -- Output: 10 30

local found_node = search_by_value(30)
if found_node ~= nil then
  print(found_node.value) -- Output: 30
else
  print("Node not found.")
end
```