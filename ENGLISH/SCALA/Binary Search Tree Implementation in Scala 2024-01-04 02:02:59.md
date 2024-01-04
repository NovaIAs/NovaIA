```scala
// This Scala program demonstrates the implementation of a binary search tree (BST) and various operations on it, including insertion, deletion, searching, and finding the minimum and maximum values.

// Defining the BinarySearchTree class
class BinarySearchTree {
  // Private inner class representing a node in the BST
  private class Node(var key: Int, var value: Int, var left: Node, var right: Node)

  // Root node of the BST
  private var root: Node = null

  // Insert a new node with the given key and value into the BST
  def insert(key: Int, value: Int): Unit = {
    // Create a new node with the given key and value
    val newNode = new Node(key, value, null, null)

    // If the BST is empty, set the root node to the new node
    if (root == null) {
      root = newNode
      return
    }

    // Otherwise, find the appropriate place to insert the new node using recursive helper method
    insertHelper(newNode, root)
  }

  // Helper method to insert a new node into the BST recursively
  private def insertHelper(newNode: Node, current: Node): Unit = {
    // If the new node's key is less than the current node's key, go to the left subtree
    if (newNode.key < current.key) {
      // If the left subtree is empty, insert the new node as the left child
      if (current.left == null) {
        current.left = newNode
        return
      } else {
        // Otherwise, recursively call insertHelper on the left subtree
        insertHelper(newNode, current.left)
      }
    } else {
      // If the new node's key is greater than or equal to the current node's key, go to the right subtree
      if (current.right == null) {
        // If the right subtree is empty, insert the new node as the right child
        current.right = newNode
        return
      } else {
        // Otherwise, recursively call insertHelper on the right subtree
        insertHelper(newNode, current.right)
      }
    }
  }

  // Search for a node with the given key in the BST
  def search(key: Int): Int = {
    // Call the searchHelper method to search for the node
    searchHelper(key, root)
  }

  // Helper method to search for a node with the given key recursively
  private def searchHelper(key: Int, current: Node): Int = {
    // If the current node is null, the key is not in the BST
    if (current == null) {
      return -1
    }

    // If the current node's key is equal to the given key, return the value associated with the node
    if (current.key == key) {
      return current.value
    }

    // If the given key is less than the current node's key, search the left subtree
    if (key < current.key) {
      return searchHelper(key, current.left)
    } else {
      // Otherwise, search the right subtree
      return searchHelper(key, current.right)
    }
  }

  // Delete a node with the given key from the BST
  def delete(key: Int): Unit = {
    // Call the deleteHelper method to delete the node
    deleteHelper(key, root, null)
  }

  // Helper method to delete a node with the given key recursively
  private def deleteHelper(key: Int, current: Node, parent: Node): Unit = {
    // If the current node is null, the key is not in the BST
    if (current == null) {
      return
    }

    // If the given key is less than the current node's key, search the left subtree
    if (key < current.key) {
      deleteHelper(key, current.left, current)
    } else if (key > current.key) {
      // If the given key is greater than the current node's key, search the right subtree
      deleteHelper(key, current.right, current)
    } else {
      // If the given key is equal to the current node's key, delete the node
      if (current.left == null && current.right == null) {
        // If the node has no children, simply remove it from the parent node
        if (parent == null) {
          root = null
        } else if (parent.left == current) {
          parent.left = null
        } else {
          parent.right = null
        }
      } else if (current.left == null) {
        // If the node has only a right child, replace the node with its right child
        if (parent == null) {
          root = current.right
        } else if (parent.left == current) {
          parent.left = current.right
        } else {
          parent.right = current.right
        }
      } else if (current.right == null) {
        // If the node has only a left child, replace the node with its left child
        if (parent == null) {
          root = current.left
        } else if (parent.left == current) {
          parent.left = current.left
        } else {
          parent.right = current.left
        }
      } else {
        // If the node has two children, find the smallest node in the right subtree and replace the current node with that node
        var successor = current.right
        while (successor.left != null) {
          successor = successor.left
        }

        current.key = successor.key
        current.value = successor.value
        deleteHelper(successor.key, current.right, current)
      }
    }
  }

  // Find the minimum value in the BST
  def findMin(): Int = {
    // Call the findMinHelper method to find the minimum value
    findMinHelper(root)
  }

  // Helper method to find the minimum value in the BST recursively
  private def findMinHelper(current: Node): Int = {
    // If the current node is null, return Integer.MAX_VALUE
    if (current == null) {
      return Integer.MAX_VALUE
    }

    // If the current node has no left child, it is the minimum value
    if (current.left == null) {
      return current.key
    } else {
      // Otherwise, recursively call findMinHelper on the left subtree
      return findMinHelper(current.left)
    }
  }

  // Find the maximum value in the BST
  def findMax(): Int = {
    // Call the findMaxHelper method to find the maximum value
    findMaxHelper(root)
  }

  // Helper method to find the maximum value in the BST recursively
  private def findMaxHelper(current: Node): Int = {
    // If the current node is null, return Integer.MIN_VALUE
    if (current == null) {
      return Integer.MIN_VALUE
    }

    // If the current node has no right child, it is the maximum value
    if (current.right == null) {
      return current.key
    } else {
      // Otherwise, recursively call findMaxHelper on the right subtree
      return findMaxHelper(current.right)
    }
  }

  // Print the BST in ascending order using inorder traversal
  def printInorder(): Unit = {
    // Call the printInorderHelper method to print the BST in ascending order
    printInorderHelper(root)
  }

  // Helper method to print the BST in ascending order using inorder traversal recursively
  private def printInorderHelper(current: Node): Unit = {
    // If the current node is not null, print its value and recursively call printInorderHelper on its left and right subtrees
    if (current != null) {
      printInorderHelper(current.left)
      println(current.key + " - " + current.value)
      printInorderHelper(current.right)
    }
  }
}

// Example usage of the BinarySearchTree class
object BinarySearchTreeExample {
  def main(args: Array[String]): Unit = {
    // Create a new binary search tree
    val bst = new BinarySearchTree()

    // Insert some values into the BST
    bst.insert(10, "Ten")
    bst.insert(5, "Five")
    bst.insert(15, "Fifteen")
    bst.insert(2, "Two")
    bst.insert(7, "Seven")
    bst.insert(12, "Twelve")
    bst.insert(20, "Twenty")

    // Search for a value in the BST
    val value = bst.search(12)
    println(s"The value associated with key 12 is: $value")

    // Delete a value from the BST
    bst.delete(15)

    // Find the minimum and maximum values in