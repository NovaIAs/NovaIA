```scala
// Class to represent a single node in the binary tree
class Node(data: Int, var left: Node = null, var right: Node = null) {
  // Data stored in this node
  val value: Int = data
}

// Class to represent a binary search tree
class BinarySearchTree {
  // Root node of the tree
  var root: Node = null

  // Insert a value into the tree
  def insert(value: Int): Unit = {
    // If the tree is empty, create a new root node
    if (root == null) {
      root = new Node(value)
    }
    // Otherwise, recursively insert the value into the correct subtree
    else {
      insertHelper(value, root)
    }
  }

  // Helper method to recursively insert a value into the correct subtree
  private def insertHelper(value: Int, node: Node): Unit = {
    // If the value is less than the current node's value, go left
    if (value < node.value) {
      // If the left subtree is empty, create a new node there
      if (node.left == null) {
        node.left = new Node(value)
      }
      // Otherwise, recursively insert the value into the left subtree
      else {
        insertHelper(value, node.left)
      }
    }
    // Otherwise, go right
    else {
      // If the right subtree is empty, create a new node there
      if (node.right == null) {
        node.right = new Node(value)
      }
      // Otherwise, recursively insert the value into the right subtree
      else {
        insertHelper(value, node.right)
      }
    }
  }

  // Search for a value in the tree
  def search(value: Int): Boolean = {
    // Start the search at the root node
    var current = root

    // While the current node is not null and the value has not been found
    while (current != null && current.value != value) {
      // If the value is less than the current node's value, go left
      if (value < current.value) {
        current = current.left
      }
      // Otherwise, go right
      else {
        current = current.right
      }
    }

    // If the current node is null, the value was not found
    if (current == null) {
      false
    }
    // Otherwise, the value was found
    else {
      true
    }
  }

  // Delete a value from the tree
  def delete(value: Int): Unit = {
    // Start the search at the root node
    var current = root
    var parent: Node = null

    // While the current node is not null and the value has not been found
    while (current != null && current.value != value) {
      // Keep track of the parent node
      parent = current

      // If the value is less than the current node's value, go left
      if (value < current.value) {
        current = current.left
      }
      // Otherwise, go right
      else {
        current = current.right
      }
    }

    // If the current node is null, the value was not found
    if (current == null) {
      println("Value not found")
    }
    // Otherwise, delete the value
    else {
      // If the current node has no children, simply remove it
      if (current.left == null && current.right == null) {
        // If the current node is the root node, set the root to null
        if (parent == null) {
          root = null
        }
        // Otherwise, remove the current node from its parent
        else {
          if (parent.left == current) {
            parent.left = null
          }
          else {
            parent.right = null
          }
        }
      }
      // If the current node has one child, replace it with its child
      else if (current.left == null) {
        // If the current node is the root node, set the root to its right child
        if (parent == null) {
          root = current.right
        }
        // Otherwise, replace the current node with its right child
        else {
          if (parent.left == current) {
            parent.left = current.right
          }
          else {
            parent.right = current.right
          }
        }
      }
      else if (current.right == null) {
        // If the current node is the root node, set the root to its left child
        if (parent == null) {
          root = current.left
        }
        // Otherwise, replace the current node with its left child
        else {
          if (parent.left == current) {
            parent.left = current.left
          }
          else {
            parent.right = current.left
          }
        }
      }
      // If the current node has two children, find its successor and replace it with the successor
      else {
        // Find the successor of the current node
        var successor = current.right
        var successorParent: Node = current

        // While the successor has a left child, keep moving left
        while (successor.left != null) {
          successorParent = successor
          successor = successor.left
        }

        // Replace the current node with the successor
        current.value = successor.value

        // Remove the successor from its parent
        if (successorParent.left == successor) {
          successorParent.left = successor.right
        }
        else {
          successorParent.right = successor.right
        }
      }
    }
  }

  // Print the tree in order (left, root, right)
  def printInOrder(): Unit = {
    printInOrderHelper(root)
  }

  // Helper method to print the tree in order
  private def printInOrderHelper(node: Node): Unit = {
    // If the node is not null, print its value and recursively print its left and right subtrees
    if (node != null) {
      printInOrderHelper(node.left)
      print(node.value + " ")
      printInOrderHelper(node.right)
    }
  }
}

// Create a binary search tree
val tree = new BinarySearchTree()

// Insert some values into the tree
tree.insert(50)
tree.insert(30)
tree.insert(70)
tree.insert(20)
tree.insert(40)
tree.insert(60)
tree.insert(80)

// Search for a value in the tree
val valueToSearch = 40
if (tree.search(valueToSearch)) {
  println(s"$valueToSearch found in the tree")
}
else {
  println(s"$valueToSearch not found in the tree")
}

// Delete a value from the tree
val valueToDelete = 20
tree.delete(valueToDelete)

// Print the tree in order
println("Tree in order:")
tree.printInOrder()
```