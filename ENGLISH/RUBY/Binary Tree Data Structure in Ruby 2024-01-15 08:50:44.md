```ruby
# Define a module for managing and manipulating complex data structures
module ComplexDataStructures

  # Define a class for representing a binary tree data structure
  class BinaryTree

    # Initialize a new binary tree with a given root node
    def initialize(root_node)
      @root_node = root_node
    end

    # Insert a new node with a given value into the binary tree
    def insert(value)
      # Create a new node with the given value
      new_node = Node.new(value)

      # If the tree is empty, set the new node as the root node
      if @root_node.nil?
        @root_node = new_node
        return
      end

      # Otherwise, find the appropriate place to insert the new node
      current_node = @root_node
      while true
        # If the value is less than the current node's value, go left
        if value < current_node.value
          # If the left child is nil, insert the new node as the left child
          if current_node.left_child.nil?
            current_node.left_child = new_node
            return
          else
            # Otherwise, move to the left child and continue the search
            current_node = current_node.left_child
          end
        # Otherwise, if the value is greater than the current node's value, go right
        elsif value > current_node.value
          # If the right child is nil, insert the new node as the right child
          if current_node.right_child.nil?
            current_node.right_child = new_node
            return
          else
            # Otherwise, move to the right child and continue the search
            current_node = current_node.right_child
          end
        # If the value is equal to the current node's value, do nothing (no duplicates allowed)
        else
          return
        end
      end
    end

    # Search for a node with a given value in the binary tree
    def search(value)
      # Initialize the current node to the root node
      current_node = @root_node

      # While the current node is not nil and the value has not been found
      while !current_node.nil? && current_node.value != value
        # If the value is less than the current node's value, move to the left child
        if value < current_node.value
          current_node = current_node.left_child
        # Otherwise, if the value is greater than the current node's value, move to the right child
        else
          current_node = current_node.right_child
        end
      end

      # Return the current node if the value was found, or nil if the value was not found
      return current_node
    end

    # Delete a node with a given value from the binary tree
    def delete(value)
      # Find the node to be deleted
      node_to_delete = search(value)

      # If the node to be deleted is nil, return (nothing to delete)
      return if node_to_delete.nil?

      # If the node to be deleted has no children, simply remove it from its parent
      if node_to_delete.left_child.nil? && node_to_delete.right_child.nil?
        # If the node to be deleted is the root node, set the root node to nil
        if node_to_delete == @root_node
          @root_node = nil
        # Otherwise, remove the node from its parent's child list
        else
          node_to_delete.parent.left_child = nil if node_to_delete == node_to_delete.parent.left_child
          node_to_delete.parent.right_child = nil if node_to_delete == node_to_delete.parent.right_child
        end

      # If the node to be deleted has only one child, replace it with its child
      elsif node_to_delete.left_child.nil?
        # If the node to be deleted is the root node, set the root node to its right child
        if node_to_delete == @root_node
          @root_node = node_to_delete.right_child
        # Otherwise, replace the node with its right child in its parent's child list
        else
          node_to_delete.parent.left_child = node_to_delete.right_child if node_to_delete == node_to_delete.parent.left_child
          node_to_delete.parent.right_child = node_to_delete.right_child if node_to_delete == node_to_delete.parent.right_child
        end

      elsif node_to_delete.right_child.nil?
        # If the node to be deleted is the root node, set the root node to its left child
        if node_to_delete == @root_node
          @root_node = node_to_delete.left_child
        # Otherwise, replace the node with its left child in its parent's child list
        else
          node_to_delete.parent.left_child = node_to_delete.left_child if node_to_delete == node_to_delete.parent.left_child
          node_to_delete.parent.right_child = node_to_delete.left_child if node_to_delete == node_to_delete.parent.right_child
        end

      # If the node to be deleted has two children, find its successor and replace the node with its successor
      else
        # Find the successor of the node to be deleted
        successor = find_successor(node_to_delete)

        # Replace the node to be deleted with its successor
        node_to_delete.value = successor.value

        # Delete the successor from the tree
        delete(successor.value)
      end
    end

    # Find the successor of a given node in the binary tree
    def find_successor(node)
      # If the node has a right child, the successor is the leftmost node in the right subtree
      if !node.right_child.nil?
        successor = node.right_child
        while !successor.left_child.nil?
          successor = successor.left_child
        end
        return successor

      # Otherwise, the successor is the lowest ancestor of the node whose left child is the node
      else
        successor = node.parent
        while !successor.nil? && successor.left_child != node
          successor = successor.parent
        end
        return successor
      end
    end

    # Print the binary tree in a human-readable format
    def print_tree
      # Use a recursive helper function to print the tree
      def print_subtree(node, level)
        # If the node is nil, return
        return if node.nil?

        # Print the node's value and level
        puts "Node value: #{node.value}, Level: #{level}"

        # Print the left subtree
        print_subtree(node.left_child, level + 1)

        # Print the right subtree
        print_subtree(node.right_child, level + 1)
      end

      # Print the root node
      print_subtree(@root_node, 0)
    end

  end

  # Define a class to represent a node in a binary tree
  class Node

    # Initialize a new node with a given value
    def initialize(value)
      @value = value
      @left_child = nil
      @right_child = nil
      @parent = nil
    end

    # Set the left child of the node
    def left_child=(node)
      @left_child = node
      node.parent = self if !node.nil?
    end

    # Set the right child of the node
    def right_child=(node)
      @right_child = node
      node.parent = self if !node.nil?
    end

    # Set the parent of the node
    def parent=(node)
      @parent = node
    end

  end

end

# Usage:
# Create a new binary tree
tree = ComplexDataStructures::BinaryTree.new(10)

# Insert some values into the tree
tree.insert(5)
tree.insert(15)
tree.insert(2)
tree.insert(7)
tree.insert(12)
tree.insert(20)

# Search for a value in the tree
found_node = tree.search(12)
puts "Found node with value 12: #{found_node.value}"

# Delete a value from the tree
tree.delete(7)

# Print the tree
tree.print_tree
```