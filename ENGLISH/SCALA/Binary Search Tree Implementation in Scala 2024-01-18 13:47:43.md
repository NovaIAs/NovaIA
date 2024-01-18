```scala
// Define a custom class for a binary tree node
class Node[T](var data: T, var left: Node[T] = null, var right: Node[T] = null)

// Define a class to represent a binary search tree
class BinarySearchTree[T extends Comparable[T]] {

  // Define the root node of the tree
  private var root: Node[T] = null

  // Insert a new value into the tree
  def insert(value: T): Unit = {
    root = insertHelper(root, value)
  }

  // Helper function to recursively insert a value into the tree
  private def insertHelper(node: Node[T], value: T): Node[T] = {
    if (node == null) {
      // If the current node is null, create a new node with the given value
      new Node[T](value)
    } else if (value.compareTo(node.data) < 0) {
      // If the value is less than the current node's data, insert it into the left subtree
      node.left = insertHelper(node.left, value)
    } else {
      // Otherwise, insert it into the right subtree
      node.right = insertHelper(node.right, value)
    }
    node
  }

  // Search for a value in the tree
  def search(value: T): Boolean = {
    searchHelper(root, value)
  }

  // Helper function to recursively search for a value in the tree
  private def searchHelper(node: Node[T], value: T): Boolean = {
    if (node == null) {
      // If the current node is null, the value is not in the tree
      false
    } else if (value.compareTo(node.data) == 0) {
      // If the value is equal to the current node's data, the value is in the tree
      true
    } else if (value.compareTo(node.data) < 0) {
      // If the value is less than the current node's data, search the left subtree
      searchHelper(node.left, value)
    } else {
      // Otherwise, search the right subtree
      searchHelper(node.right, value)
    }
  }

  // Delete a value from the tree
  def delete(value: T): Unit = {
    root = deleteHelper(root, value)
  }

  // Helper function to recursively delete a value from the tree
  private def deleteHelper(node: Node[T], value: T): Node[T] = {
    if (node == null) {
      // If the current node is null, the value is not in the tree, so do nothing
      null
    } else if (value.compareTo(node.data) < 0) {
      // If the value is less than the current node's data, delete it from the left subtree
      node.left = deleteHelper(node.left, value)
    } else if (value.compareTo(node.data) > 0) {
      // If the value is greater than the current node's data, delete it from the right subtree
      node.right = deleteHelper(node.right, value)
    } else {
      // If the value is equal to the current node's data, delete the current node
      if (node.left == null) {
        // If the current node has no left child, replace it with its right child
        node.right
      } else if (node.right == null) {
        // If the current node has no right child, replace it with its left child
        node.left
      } else {
        // If the current node has both a left and a right child, find the smallest value in the right subtree
        // and replace the current node with that value
        val minValue = findMinHelper(node.right)
        node.data = minValue.data
        node.right = deleteHelper(node.right, minValue.data)
      }
    }
    node
  }

  // Find the minimum value in the tree
  def findMin(): T = {
    findMinHelper(root).data
  }

  // Helper function to recursively find the minimum value in the tree
  private def findMinHelper(node: Node[T]): Node[T] = {
    if (node.left == null) {
      // If the current node has no left child, it is the minimum value
      node
    } else {
      // Otherwise, find the minimum value in the left subtree
      findMinHelper(node.left)
    }
  }

  // Find the maximum value in the tree
  def findMax(): T = {
    findMaxHelper(root).data
  }

  // Helper function to recursively find the maximum value in the tree
  private def findMaxHelper(node: Node[T]): Node[T] = {
    if (node.right == null) {
      // If the current node has no right child, it is the maximum value
      node
    } else {
      // Otherwise, find the maximum value in the right subtree
      findMaxHelper(node.right)
    }
  }

  // Print the tree in preorder traversal order
  def preorder(): Unit = {
    preorderHelper(root)
  }

  // Helper function to recursively print the tree in preorder traversal order
  private def preorderHelper(node: Node[T]): Unit = {
    if (node != null) {
      print(node.data + " ")
      preorderHelper(node.left)
      preorderHelper(node.right)
    }
  }

  // Print the tree in inorder traversal order
  def inorder(): Unit = {
    inorderHelper(root)
  }

  // Helper function to recursively print the tree in inorder traversal order
  private def inorderHelper(node: Node[T]): Unit = {
    if (node != null) {
      inorderHelper(node.left)
      print(node.data + " ")
      inorderHelper(node.right)
    }
  }

  // Print the tree in postorder traversal order
  def postorder(): Unit = {
    postorderHelper(root)
  }

  // Helper function to recursively print the tree in postorder traversal order
  private def postorderHelper(node: Node[T]): Unit = {
    if (node != null) {
      postorderHelper(node.left)
