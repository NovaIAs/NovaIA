```kotlin
// A custom class to represent a binary tree node
class Node<T>(var value: T, var left: Node<T>? = null, var right: Node<T>? = null) {

    // Function to check if a node is a leaf node (has no children)
    fun isLeaf(): Boolean {
        return left == null && right == null
    }
}

// A custom class to represent a binary search tree
class BinarySearchTree<T : Comparable<T>> {

    // The root node of the tree
    private var root: Node<T>? = null

    // Function to insert a new value into the tree
    fun insert(value: T) {
        root = insert(value, root)
    }

    // Helper function to insert a new value into the tree recursively
    private fun insert(value: T, node: Node<T>?): Node<T> {
        if (node == null) {
            return Node(value)
        }
        if (value < node.value) {
            node.left = insert(value, node.left)
        } else {
            node.right = insert(value, node.right)
        }
        return node
    }

    // Function to search for a value in the tree
    fun search(value: T): Node<T>? {
        return search(value, root)
    }

    // Helper function to search for a value in the tree recursively
    private fun search(value: T, node: Node<T>?): Node<T>? {
        if (node == null) {
            return null
        }
        if (value == node.value) {
            return node
        }
        if (value < node.value) {
            return search(value, node.left)
        } else {
            return search(value, node.right)
        }
    }

    // Function to find the minimum value in the tree
    fun findMin(): T? {
        var node = root
        while (node?.left != null) {
            node = node.left
        }
        return node?.value
    }

    // Function to find the maximum value in the tree
    fun findMax(): T? {
        var node = root
        while (node?.right != null) {
            node = node.right
        }
        return node?.value
    }

    // Function to delete a value from the tree
    fun delete(value: T) {
        root = delete(value, root)
    }

    // Helper function to delete a value from the tree recursively
    private fun delete(value: T, node: Node<T>?): Node<T>? {
        if (node == null) {
            return null
        }
        if (value < node.value) {
            node.left = delete(value, node.left)
        } else if (value > node.value) {
            node.right = delete(value, node.right)
        } else {
            if (node.isLeaf()) {
                return null
            } else if (node.left == null) {
                return node.right
            } else if (node.right == null) {
                return node.left
            } else {
                node.value = findMin(node.right)!!
                node.right = delete(node.value, node.right)
            }
        }
        return node
    }

    // Function to check if the tree is empty
    fun isEmpty(): Boolean {
        return root == null
    }

    // Function to get the size of the tree (the number of nodes)
    fun size(): Int {
        return size(root)
    }

    // Helper function to get the size of the tree recursively
    private fun size(node: Node<T>?): Int {
        if (node == null) {
            return 0
        }
        return 1 + size(node.left) + size(node.right)
    }

    // Function to print the tree in a pre-order traversal (root, left, right)
    fun printPreOrder() {
        printPreOrder(root)
    }

    // Helper function to print the tree in a pre-order traversal recursively
    private fun printPreOrder(node: Node<T>?) {
        if (node == null) {
            return
        }
        print("${node.value} ")
        printPreOrder(node.left)
        printPreOrder(node.right)
    }

    // Function to print the tree in an in-order traversal (left, root, right)
    fun printInOrder() {
        printInOrder(root)
    }

    // Helper function to print the tree in an in-order traversal recursively
    private fun printInOrder(node: Node<T>?) {
        if (node == null) {
            return
        }
        printInOrder(node.left)
        print("${node.value} ")
        printInOrder(node.right)
    }

    // Function to print the tree in a post-order traversal (left, right, root)
    fun printPostOrder() {
        printPostOrder(root)
    }

    // Helper function to print the tree in a post-order traversal recursively
    private fun printPostOrder(node: Node<T>?) {
        if (node == null) {
            return
        }
        printPostOrder(node.left)
        printPostOrder(node.right)
        print("${node.value} ")
    }
}

// Usage example
val tree = BinarySearchTree<Int>()
tree.insert(10)
tree.insert(5)
tree.insert(15)
tree.insert(2)
tree.insert(7)
tree.insert(12)
tree.insert(20)

println("Pre-order traversal:")
tree.printPreOrder()
println()

println("In-order traversal:")
tree.printInOrder()
println()

println("Post-order traversal:")
tree.printPostOrder()
println()

println("Minimum value in the tree: ${tree.findMin()}")
println("Maximum value in the tree: ${tree.findMax()}")
println("Size of the tree: ${tree.size()}")

val node = tree.search(12)
if (node != null) {
    println("Found node with value 12")
} else {
    println("Node with value 12 not found")
}

tree.delete(15)
println("Deleted node with value 15")

println("Pre-order traversal after deletion:")
tree.printPreOrder()
println()

println("Size of the tree after deletion: ${tree.size()}")
```

This code implements a binary search tree in Kotlin. A binary search tree is a data structure that stores data in a way that allows for efficient searching and retrieval. It is made up of nodes, where each node has a value and two pointers to other nodes, called the left child and the right child.

The code includes the following methods:

* `insert()`: Inserts a new value into the tree.
* `search()`: Searches for a value in the tree.
* `findMin()`: Finds the minimum value in the tree.
* `findMax()`: Finds the maximum value in the tree.
* `delete()`: Deletes a value from the tree.
* `isEmpty()`: Checks if the tree is empty.
* `size()`: Gets the size of the tree (the number of nodes).
* `printPreOrder()`: Prints the tree in a pre-order traversal (root, left, right).
* `printInOrder()`: Prints the tree in an in-order traversal (left, root, right).
* `printPostOrder()`: Prints the tree in a post-order traversal (left, right, root).

The usage example shows how to create a binary search tree, insert values into it, search for a value, find the minimum and maximum values, delete a value, and print the tree in different traversal orders.