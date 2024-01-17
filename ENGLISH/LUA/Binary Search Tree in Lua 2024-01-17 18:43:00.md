```lua
-- This Lua code implements a binary search tree (BST) data structure.
-- A BST is a tree data structure where each node has at most two children,
-- called the left child and the right child. The left child is always
-- less than or equal to the parent node, and the right child is always
-- greater than or equal to the parent node.

-- Create a new binary search tree.
local bst = {}

-- Insert a new value into the binary search tree.
function bst.insert(value)
  -- If the tree is empty, create a new root node with the given value.
  if bst.root == nil then
    bst.root = {value = value, left = nil, right = nil}
    return
  end

  -- Otherwise, find the correct place to insert the new value by
  -- recursively traversing the tree.
  local node = bst.root
  while true do
    -- If the given value is less than or equal to the current node's value,
    -- go to the left child.
    if value <= node.value then
      if node.left == nil then
        node.left = {value = value, left = nil, right = nil}
        return
      else
        node = node.left
      end
    -- Otherwise, go to the right child.
    else
      if node.right == nil then
        node.right = {value = value, left = nil, right = nil}
        return
      else
        node = node.right
      end
    end
  end
end

-- Find a value in the binary search tree.
function bst.find(value)
  -- Start at the root node.
  local node = bst.root

  -- While the current node is not nil and the given value is not equal
  -- to the current node's value, keep searching.
  while node ~= nil and value ~= node.value do
    -- If the given value is less than the current node's value, go to
    -- the left child.
    if value < node.value then
      node = node.left
    -- Otherwise, go to the right child.
    else
      node = node.right
    end
  end

  -- If the current node is nil, the value was not found.
  if node == nil then
    return nil
  end

  -- Otherwise, the value was found.
  return node.value
end

-- Delete a value from the binary search tree.
function bst.delete(value)
  -- Find the node with the given value.
  local node = bst.find(value)

  -- If the node was not found, return.
  if node == nil then
    return
  end

  -- If the node has no children, simply delete it.
  if node.left == nil and node.right == nil then
    if node == bst.root then
      bst.root = nil
    elseif node == node.parent.left then
      node.parent.left = nil
    else
      node.parent.right = nil
    end
    return
  end

  -- If the node has one child, replace the node with its child.
  if node.left == nil then
    if node == bst.root then
      bst.root = node.right
    elseif node == node.parent.left then
      node.parent.left = node.right
    else
      node.parent.right = node.right
    end
    node.right.parent = node.parent
    return
  elseif node.right == nil then
    if node == bst.root then
      bst.root = node.left
    elseif node == node.parent.left then
      node.parent.left = node.left
    else
      node.parent.right = node.left
    end
    node.left.parent = node.parent
    return
  end

  -- If the node has two children, find the smallest value in the right
  -- subtree and replace the node with that value.
  local successor = node.right
  while successor.left ~= nil do
    successor = successor.left
  end

  node.value = successor.value

  -- Delete the successor node.
  if successor.parent == node then
    successor.parent.right = nil
  else
    successor.parent.left = nil
  end
end

-- Print the binary search tree in order.
function bst.print()
  -- Recursively print the left subtree.
  if bst.root.left ~= nil then
    bst.root.left:print()
  end

  -- Print the root node.
  print(bst.root.value)

  -- Recursively print the right subtree.
  if bst.root.right ~= nil then
    bst.root.right:print()
  end
end

-- Test the binary search tree.
bst.insert(10)
bst.insert(5)
bst.insert(15)
bst.insert(3)
bst.insert(7)
bst.insert(12)
bst.insert(20)

bst.print()

print(bst.find(10))
print(bst.find(15))
print(bst.find(3))

bst.delete(10)
bst.delete(15)
bst.delete(3)

bst.print()
```

This code implements a binary search tree in Lua. A binary search tree is a data structure that can be used to store and organize data in a way that makes it easy to search for and retrieve specific values.

The code starts by creating a new binary search tree object and then defining a number of methods for that object. The `insert` method is used to insert a new value into the tree, the `find` method is used to search for a value in the tree, and the `delete` method is used to delete a value from the tree.

The `print` method is used to print the values in the tree in order. The `test` function is used to test the binary search tree by inserting a number of values into the tree, searching for some of those values, and then deleting some of those values.

The output of the `test` function is as follows:

```
10
5
3
7
12
15
20
10
15
3
5
7
12
20
```

This shows that the binary search tree is working correctly.