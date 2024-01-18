```scala
// Define a data structure to represent a binary tree node
case class Node[A](value: A, left: Option[Node[A]], right: Option[Node[A]])

// Define a function to create a binary tree from a list of values
def createTree[A](values: List[A]): Option[Node[A]] = {
  // If the list is empty, return None
  if (values.isEmpty) None
  // Otherwise, create a new node with the first value in the list as the value
  else Some(Node(values.head, createTree(values.tail), createTree(values.tail.tail)))
}

// Define a function to search for a value in a binary tree
def searchTree[A](value: A, tree: Option[Node[A]]): Boolean = {
  // If the tree is empty, return false
  if (tree.isEmpty) false
  // Otherwise, check if the value is equal to the value of the current node
  else if (tree.get.value == value) true
  // Otherwise, recursively search the left and right subtrees
  else searchTree(value, tree.get.left) || searchTree(value, tree.get.right)
}

// Define a function to insert a value into a binary tree
def insertTree[A](value: A, tree: Option[Node[A]]): Option[Node[A]] = {
  // If the tree is empty, create a new node with the value as the value
  if (tree.isEmpty) Some(Node(value, None, None))
  // Otherwise, if the value is less than the value of the current node, insert it into the left subtree
  else if (value < tree.get.value) Some(Node(tree.get.value, insertTree(value, tree.get.left), tree.get.right))
  // Otherwise, insert it into the right subtree
  else Some(Node(tree.get.value, tree.get.left, insertTree(value, tree.get.right)))
}

// Define a function to delete a value from a binary tree
def deleteTree[A](value: A, tree: Option[Node[A]]): Option[Node[A]] = {
  // If the tree is empty, return None
  if (tree.isEmpty) None
  // Otherwise, if the value is equal to the value of the current node, delete the node
  else if (tree.get.value == value) {
    // If the node has no children, return None
    if (tree.get.left.isEmpty && tree.get.right.isEmpty) None
    // If the node has only one child, return the child
    else if (tree.get.left.isEmpty) tree.get.right
    else if (tree.get.right.isEmpty) tree.get.left
    // Otherwise, find the smallest value in the right subtree and replace the current node with it
    else {
      val minValue = findMinValue(tree.get.right)
      Some(Node(minValue, deleteTree(minValue, tree.get.right), tree.get.right))
    }
  }
  // Otherwise, recursively search the left and right subtrees
  else if (value < tree.get.value) Some(Node(tree.get.value, deleteTree(value, tree.get.left), tree.get.right))
  else Some(Node(tree.get.value, tree.get.left, deleteTree(value, tree.get.right)))
}

// Define a function to find the minimum value in a binary tree
def findMinValue[A](tree: Option[Node[A]]): A = {
  // If the tree is empty, return the default value
  if (tree.isEmpty) 0.asInstanceOf[A]
  // Otherwise, recursively search the left subtree
  else findMinValue(tree.get.left)
}

// Define a function to find the maximum value in a binary tree
def findMaxValue[A](tree: Option[Node[A]]): A = {
  // If the tree is empty, return the default value
  if (tree.isEmpty) 0.asInstanceOf[A]
  // Otherwise, recursively search the right subtree
  else findMaxValue(tree.get.right)
}

// Define a function to find the height of a binary tree
def findHeight[A](tree: Option[Node[A]]): Int = {
  // If the tree is empty, return 0
  if (tree.isEmpty) 0
  // Otherwise, find the height of the left and right subtrees and return the maximum height + 1
  else 1 + math.max(findHeight(tree.get.left), findHeight(tree.get.right))
}

// Define a function to print a binary tree in a 2D representation
def printTree[A](tree: Option[