```kotlin
// This is a Kotlin program that implements a simple binary search tree.
// A binary search tree is a data structure that stores data in a way that allows for efficient searching and retrieval.
// It consists of a root node and a set of child nodes, which are organized in a hierarchical structure.
// Each node contains a key and a value, and the keys are ordered in ascending order.

// This class represents a node in the binary search tree.
class Node<T>(var key: Int, var value: T) {
    var left: Node<T>? = null
    var right: Node<T>? = null
}

// This class represents the binary search tree.
class BinarySearchTree<T> {
    var root: Node<T>? = null

    // This method inserts a new node into the binary search tree.
    // If the tree is empty, the new node becomes the root node.
    // Otherwise, the new node is inserted into the appropriate child node of the root node.
    fun insert(key: Int, value: T) {
        if (root == null) {
            root = Node(key, value)
        } else {
            insertNode(root!!, key, value)
        }
    }

    // This method inserts a new node into the binary search tree.
    // It takes the current node, the key of the new node, and the value of the new node as arguments.
    // If the key of the new node is less than the key of the current node, the new node is inserted into the left child node of the current node.
    // If the key of the new node is greater than the key of the current node, the new node is inserted into the right child node of the current node.
    private fun insertNode(node: Node<T>, key: Int, value: T) {
        if (key < node.key) {
            if (node.left == null) {
                node.left = Node(key, value)
            } else {
                insertNode(node.left!!, key, value)
            }
        } else {
            if (node.right == null) {
                node.right = Node(key, value)
            } else {
                insertNode(node.right!!, key, value)
            }
        }
    }

    // This method searches for a node with the given key in the binary search tree.
    // If the node is found, it returns the value of the node.
    // Otherwise, it returns null.
    fun search(key: Int): T? {
        return searchNode(root, key)
    }

    // This method searches for a node with the given key in the binary search tree.
    // It takes the current node and the key of the node to be searched as arguments.
    // If the key of the current node is equal to the key of the node to be searched, the value of the current node is returned.
    // Otherwise, if the key of the current node is greater than the key of the node to be searched, the left child node of the current node is searched.
    // If the key of the current node is less than the key of the node to be searched, the right child node of the current node is searched.
    private fun searchNode(node: Node<T>?, key: Int): T? {
        if (node == null) {
            return null
        }
        if (node.key == key) {
            return node.value
        }
        return if (key < node.key) {
            searchNode(node.left, key)
        } else {
            searchNode(node.right, key)
        }
    }

    // This method deletes a node with the given key from the binary search tree.
    // If the node is found, it is deleted from the tree and the value of the node is returned.
    // Otherwise, null is returned.
    fun delete(key: Int): T? {
        return deleteNode(root, key)
    }

    // This method deletes a node with the given key from the binary search tree.
    // It takes the current node and the key of the node to be deleted as arguments.
    // If the key of the current node is equal to the key of the node to be deleted, the node is deleted from the tree and the value of the node is returned.
    // Otherwise, if the key of the current node is greater than the key of the node to be deleted, the left child node of the current node is searched.
    // If the key of the current node is less than the key of the node to be deleted, the right child node of the current node is searched.
    private fun deleteNode(node: Node<T>?, key: Int): T? {
        if (node == null) {
            return null
        }
        if (node.key == key) {
            val value = node.value
            if (node