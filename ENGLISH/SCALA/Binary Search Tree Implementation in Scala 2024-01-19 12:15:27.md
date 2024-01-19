```scala
// Define a class representing a binary tree node
class Node[T](var value: T, var left: Node[T] = null, var right: Node[T] = null)

// Define a class representing a binary search tree
class BinarySearchTree[T](implicit ordering: Ordering[T]) {

  // Define the root node of the tree
  private var root: Node[T] = null

  // Insert a new value into the tree
  def insert(value: T): Unit = {
    if (root == null) {
      root = new Node(value)
    } else {
      insertHelper(value, root)
    }
  }

  // Helper method for inserting a new value into the tree
  private def insertHelper(value: T, node: Node[T]) = {
    if (ordering.lt(value, node.value)) {
      if (node.left == null) {
        node.left = new Node(value)
      } else {
        insertHelper(value, node.left)
      }
    } else {
      if (node.right == null) {
        node.right = new Node(value)
      } else {
        insertHelper(value, node.right)
      }
    }
  }

  // Search for a value in the tree
  def search(value: T): Boolean = {
    if (root == null) {
      false
    } else {
      searchHelper(value, root)
    }
  }

  // Helper method for searching for a value in the tree
  private def searchHelper(value: T, node: Node[T]): Boolean = {
    if (node == null) {
      false
    } else if (ordering.equiv(value, node.value)) {
      true
    } else if (ordering.lt(value, node.value)) {
      searchHelper(value, node.left)
    } else {
      searchHelper(value, node.right)
    }
  }

  // Delete a value from the tree
  def delete(value: T): Unit = {
    if (root == null) {
      return
    } else {
      deleteHelper(value, root)
    }
  }

  // Helper method for deleting a value from the tree
  private def deleteHelper(value: T, node: Node[T]): Unit = {
    if (node == null) {
      return
    } else if (ordering.lt(value, node.value)) {
      deleteHelper(value, node.left)
    } else if (ordering.gt(value, node.value)) {
      deleteHelper(value, node.right)
    } else {
      if (node.left == null) {
        node.value = node.right.value
        node.right = null
      } else if (node.right == null) {
        node.value = node.left.value
        node.left = null
      } else {
        val minNode = findMinNode(node.right)
        node.value = minNode.value
        deleteHelper(minNode.value, node.right)
      }
    }
  }

  // Find the minimum node in a subtree
  private def findMinNode(node: Node[T]): Node[T] = {
    if (node.left == null) {
      node
    } else {
      findMinNode(node.left)
    }
  }

  // Print the tree in pre-order traversal
  def preOrderTraversal(): Unit = {
    if (root == null) {
      return
    } else {
      preOrderTraversalHelper(root)
    }
  }

  // Helper method for printing the tree in pre-order traversal
  private def preOrderTraversalHelper(node: Node[T]): Unit = {
    print(node.value + " ")
    if (node.left != null) {
      preOrderTraversalHelper(node.left)
    }
    if (node.right != null) {
      preOrderTraversalHelper(node.right)
    }
  }

  // Print the tree in in-order traversal
  def inOrderTraversal(): Unit = {
    if (root == null) {
      return
    } else {
      inOrderTraversalHelper(root)
    }
  }

  // Helper method for printing the tree in in-order traversal
  private def inOrderTraversalHelper(node: Node[T]): Unit = {
    if (node.left != null) {
      inOrderTraversalHelper(node.left)
    }
    print(node.value + " ")
    if (node.right != null) {
      inOrderTraversalHelper(node.right)
    }
  }

  // Print the tree in post-order traversal
  def postOrderTraversal(): Unit = {
    if (root == null) {
      return
    } else {
      postOrderTraversalHelper(root)
    }
  }

  // Helper method for printing the tree in post-order traversal
  private def postOrderTraversalHelper(node: Node[T]): Unit = {
    if (node.left != null) {
      postOrderTraversalHelper(node.left)
    }
    if (node.right != null) {
      postOrderTraversalHelper(node.right)
    }
    print(node.value + " ")
  }
}

// Define the main method
object Main {
  def main(args: Array[String]): Unit = {
    // Create a new binary search tree
    val tree = new BinarySearchTree[Int]()

    // Insert some values into the tree
    tree.insert(10)
    tree.insert(5)
    tree.insert(15)
    tree.insert(2)
    tree.insert(7)
    tree.insert(12)
    tree.insert(20)

    // Search for a value in the tree
    val searchResult = tree.search(12)
    println(s"Search result for 12: $searchResult")

    // Delete a value from the tree
    tree.delete(15)

    // Print the tree in pre-order traversal
    tree.preOrderTraversal()
    println()

    // Print the tree in in-order traversal
    tree.inOrderTraversal()
    println()

    // Print the tree in post-order traversal
    tree.postOrderTraversal()
    println()
  }
}
```

This code defines a binary search tree data structure in Scala. It includes methods for inserting, searching, and deleting values, as well as methods for printing the tree in pre-order, in-order, and post-order traversals. The tree is implemented using a node-based approach, where each node contains a value and references to its left and right child nodes.

The code first defines a class representing a binary tree node, which contains a value and references to its left and right child nodes. The class also includes a constructor that takes a value and two optional child nodes as parameters.

Next, the code defines a class representing a binary search tree. The class includes a private root node variable, which is initially set to null. The class also includes methods for inserting, searching, and deleting values, as well as methods for printing the tree in pre-order, in-order, and post-order traversals.

The insert method takes a value as a parameter and inserts it into the tree. The method first checks if the root node is null. If it is, the value is inserted as the root node. Otherwise, the method calls a helper method to insert the value into the appropriate location in the tree.

The search method takes a value as a parameter and searches for it in the tree. The method first checks if the root node is null. If it is, the method returns false. Otherwise, the method calls a helper method to search for the value in the tree.

The delete method takes a value as a parameter and deletes it from the tree. The method first checks if the root node is null. If it is, the method returns. Otherwise, the method calls a helper method to delete the value from the tree.

The findMinNode method takes a node as a parameter and finds the minimum node in the subtree rooted at that node. The method first checks if the left child node of the given node is null. If it is, the method returns the given node. Otherwise, the method calls itself recursively on the left child node.

The preOrderTraversal, inOrderTraversal, and postOrderTraversal methods are used to print the tree in pre-order, in-order, and post-order traversals, respectively. Each method takes a node as a parameter and prints the value of the node, followed by the values of its left and right child nodes, if they exist.

Finally, the code defines a main method, which creates a new binary search tree, inserts some values into the tree, searches for a value in the tree, deletes a value from the tree, and prints the tree in pre-order, in-order, and post-order traversals.