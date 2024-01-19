```scala
// Define a class to represent a binary tree node
class Node(val value: Int) {
  // Each node has a left and right child
  var left: Node = null
  var right: Node = null
}

// Define the main object
object BinaryTree {
  // Function to insert a new node with the given value into the binary tree
  def insert(root: Node, value: Int): Unit = {
    // If the root is null, create a new node and set it as the root
    if (root == null) {
      root = new Node(value)
    }
    // If the value is less than the root's value, insert it in the left subtree
    else if (value < root.value) {
      insert(root.left, value)
    }
    // Otherwise, insert it in the right subtree
    else {
      insert(root.right, value)
    }
  }

  // Function to search for a node with the given value in the binary tree
  def search(root: Node, value: Int): Node = {
    // If the root is null, the value is not in the tree
    if (root == null) {
      return null
    }
    // If the value is equal to the root's value, return the root
    else if (value == root.value) {
      return root
    }
    // If the value is less than the root's value, search in the left subtree
    else if (value < root.value) {
      return search(root.left, value)
    }
    // Otherwise, search in the right subtree
    else {
      return search(root.right, value)
    }
  }

  // Function to delete a node with the given value from the binary tree
  def delete(root: Node, value: Int): Node = {
    // If the root is null, the value is not in the tree
    if (root == null) {
      return null
    }
    // If the value is equal to the root's value, delete the root
    else if (value == root.value) {
      // If the root has no children, set it to null
      if (root.left == null && root.right == null) {
        return null
      }
      // If the root has only one child, replace it with that child
      else if (root.left == null) {
        return root.right
      }
      else if (root.right == null) {
        return root.left
      }
      // If the root has two children, find the smallest node in the right subtree and replace the root with that node
      else {
        var minNode = root.right
        while (minNode.left != null) {
          minNode = minNode.left
        }
        root.value = minNode.value
        root.right = delete(root.right, minNode.value)
      }
    }
    // If the value is less than the root's value, delete it from the left subtree
    else if (value < root.value) {
      root.left = delete(root.left, value)
    }
    // Otherwise, delete it from the right subtree
    else {
      root.right = delete(root.right, value)
    }
    // Return the root
    return root
  }

  // Function to print the binary tree in preorder traversal
  def preorder(root: Node): Unit = {
    // If the root is null, return
    if (root == null) {
      return
    }
    // Print the root's value
    print(root.value + " ")
    // Print the left subtree in preorder traversal
    preorder(root.left)
    // Print the right subtree in preorder traversal
    preorder(root.right)
  }

  // Function to print the binary tree in inorder traversal
  def inorder(root: Node): Unit = {
    // If the root is null, return
    if (root == null) {
      return
    }
    // Print the left subtree in inorder traversal
    inorder(root.left)
    // Print the root's value
    print(root.value + " ")
    // Print the right subtree in inorder traversal
    inorder(root.right)
  }

  // Function to print the binary tree in postorder traversal
  def postorder(root: Node): Unit = {
    // If the root is null, return
    if (root == null) {
      return
    }
    // Print the left subtree in postorder traversal
    postorder(root.left)
    // Print the right subtree in postorder traversal
    postorder(root.right)
    // Print the root's value
    print(root.value + " ")
  }

  // Function to print the binary tree in level order traversal
  def levelOrder(root: Node): Unit = {
    // Create a queue to store the nodes to be processed
    val queue = new scala.collection.mutable.Queue[Node]
    // Add the root node to the queue
    queue.enqueue(root)
    // While the queue is not empty, dequeue a node, print its value, and enqueue its children
    while (!queue.isEmpty) {
      val node = queue.dequeue
      print(node.value + " ")
      if (node.left != null) {
        queue.enqueue(node.left)
      }
      if (node.right != null) {
        queue.enqueue(node.right)
      }
    }
  }

  // Function to find the height of the binary tree
  def height(root: Node): Int = {
    // If the root is null, the height is 0
    if (root == null) {
      return 0
    }
    // The height of the binary tree is the maximum height of its left and right subtrees plus 1
    return 1 + math.max(height(root.left), height(root.right))
  }

  // Function to find the minimum value in the binary tree
  def minValue(root: Node): Node = {
    // If the root is null, return null
    if (root == null) {
      return null
    }
    // The minimum value is the leftmost node in the binary tree
    var minNode = root
    while (minNode.left != null) {
      minNode = minNode.left
    }
    // Return the minimum node
    return minNode
  }

  // Function to find the maximum value in the binary tree
  def maxValue(root: Node): Node = {
    // If the root is null, return null
    if (root == null) {
      return null
    }
    // The maximum value is the rightmost node in the binary tree
    var maxNode = root
    while (maxNode.right != null) {
      maxNode = maxNode.right
    }
    // Return the maximum node
    return maxNode
  }

  // Function to check if the binary tree is balanced
  def isBalanced(root: Node): Boolean = {
    // If the root is null, the tree is balanced
    if (root == null) {
      return true
    }
    // The tree is balanced if the difference in heights of its left and right subtrees is at most 1
    val leftHeight = height(root.left)
    val rightHeight = height(root.right)
    return math.abs(leftHeight - rightHeight) <= 1 && isBalanced(root.left) && isBalanced(root.right)
  }

  // Function to convert a binary tree to a linked list
  def toLinkedList(root: Node): Node = {
    // If the root is null, return null
    if (root == null) {
      return null
    }
    // Convert the left subtree to a linked list
    val leftList = toLinkedList(root.left)
    // Convert the right subtree