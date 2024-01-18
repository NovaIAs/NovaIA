```swift
import Foundation

// Define the Node class to represent the nodes in the search tree
class Node<T: Comparable> {
    var value: T
    var leftChild: Node<T>?
    var rightChild: Node<T>?

    init(value: T) {
        self.value = value
        self.leftChild = nil
        self.rightChild = nil
    }
}

// Define the BinarySearchTree class
class BinarySearchTree<T: Comparable> {
    private var root: Node<T>?

    // Function to insert a node into the tree
    func insert(value: T) {
        // If the tree is empty, create a new root node
        guard let root = root else {
            self.root = Node(value: value)
            return
        }

        // Recursively insert the node in the appropriate subtree
        insertNode(value: value, currentNode: root)
    }

    // Function to recursively insert a node into the tree
    private func insertNode(value: T, currentNode: Node<T>) {
        // If the value is less than the current node's value, insert in the left subtree
        if value < currentNode.value {
            if let leftChild = currentNode.leftChild {
                insertNode(value: value, currentNode: leftChild)
            } else {
                currentNode.leftChild = Node(value: value)
            }
        }
        // Else, insert in the right subtree
        else {
            if let rightChild = currentNode.rightChild {
                insertNode(value: value, currentNode: rightChild)
            } else {
                currentNode.rightChild = Node(value: value)
            }
        }
    }

    // Function to search for a value in the tree
    func search(value: T) -> Bool {
        // Start the search from the root node
        guard let root = root else {
            return false
        }

        // Recursively search for the value in the appropriate subtree
        return searchNode(value: value, currentNode: root)
    }

    // Function to recursively search for a value in the tree
    private func searchNode(value: T, currentNode: Node<T>) -> Bool {
        // If the value is equal to the current node's value, return true
        if value == currentNode.value {
            return true
        }

        // If the value is less than the current node's value, search in the left subtree
        else if value < currentNode.value {
            if let leftChild = currentNode.leftChild {
                return searchNode(value: value, currentNode: leftChild)
            } else {
                return false
            }
        }
        // Else, search in the right subtree
        else {
            if let rightChild = currentNode.rightChild {
                return searchNode(value: value, currentNode: rightChild)
            } else {
                return false
            }
        }
    }

    // Function to find the minimum value in the tree
    func findMin() -> T? {
        // Start from the root node
        guard let root = root else {
            return nil
        }

        // Recursively find the minimum value in the left subtree
        return findMinNode(currentNode: root)
    }

    // Function to recursively find the minimum value in the tree
    private func findMinNode(currentNode: Node<T>) -> T {
        // If the left child is nil, the current node is the minimum
        if currentNode.leftChild == nil {
            return currentNode.value
        } else {
            // Recursively find the minimum value in the left subtree
            return findMinNode(currentNode: currentNode.leftChild!)
        }
    }

    // Function to find the maximum value in the tree
    func findMax() -> T? {
        // Start from the root node
        guard let root = root else {
            return nil
        }

        // Recursively find the maximum value in the right subtree
        return findMaxNode(currentNode: root)
    }

    // Function to recursively find the maximum value in the tree
    private func findMaxNode(currentNode: Node<T>) -> T {
        // If the right child is nil, the current node is the maximum
        if currentNode.rightChild == nil {
            return currentNode.value
        } else {
            // Recursively find the maximum value in the right subtree
            return findMaxNode(currentNode: currentNode.rightChild!)
        }
    }

    // Function to delete a value from the tree
    func delete(value: T) {
        // Start from the root node
        guard let root = root else {
            return
        }

        // Delete the node from the appropriate subtree
        deleteNode(value: value, currentNode: root)
    }

    // Function to recursively delete a value from the tree
    private func deleteNode(value: T, currentNode: Node<T>) {
        // If the value is less than the current node's value, delete from the left subtree
        if value < currentNode.value {
            if let leftChild = currentNode.leftChild {
                deleteNode(value: value, currentNode: leftChild)
            }
        }
        // Else, delete from the right subtree
        else if value > currentNode.value {
            if let rightChild = currentNode.rightChild {
                deleteNode(value: value, currentNode: rightChild)
            }
        }
        // If the value is equal to the current node's value, delete the current node
        else {
            // If the node has no children, simply delete it
            if currentNode.leftChild == nil && currentNode.rightChild == nil {
                currentNode = nil
            }
            // If the node has only one child, replace the current node with its child
            else if currentNode.leftChild == nil {
                currentNode = currentNode.rightChild
            } else if currentNode.rightChild == nil {
                currentNode = currentNode.leftChild