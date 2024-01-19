```swift
// Class to define a custom error type
enum MyError: Error {
    case invalidInput
    case operationFailed
}

// Class to define a data structure (a node) for a binary search tree
class Node<T: Comparable> {
    var value: T
    var leftChild: Node<T>?
    var rightChild: Node<T>?

    init(value: T) {
        self.value = value
    }
}

// Class to implement a binary search tree
class BinarySearchTree<T: Comparable> {
    private var rootNode: Node<T>?

    // Function to insert a value into the binary search tree
    func insert(_ value: T) {
        if let root = rootNode {
            insertNode(value, to: root)
        } else {
            rootNode = Node(value: value)
        }
    }

    // Helper function to insert a value into the binary search tree recursively
    private func insertNode(_ value: T, to node: Node<T>) {
        if value < node.value {
            if let leftChild = node.leftChild {
                insertNode(value, to: leftChild)
            } else {
                node.leftChild = Node(value: value)
            }
        } else if value > node.value {
            if let rightChild = node.rightChild {
                insertNode(value, to: rightChild)
            } else {
                node.rightChild = Node(value: value)
            }
        }
    }

    // Function to search for a value in the binary search tree
    func search(_ value: T) -> Node<T>? {
        guard let root = rootNode else {
            return nil
        }

        var currentNode = root
        while currentNode != nil {
            if value == currentNode.value {
                return currentNode
            } else if value < currentNode.value {
                currentNode = currentNode.leftChild
            } else {
                currentNode = currentNode.rightChild
            }
        }

        return nil
    }

    // Function to delete a value from the binary search tree
    func delete(_ value: T) throws {
        guard let root = rootNode else {
            throw MyError.invalidInput
        }

        var parentNode: Node<T>?
        var currentNode = root

        while currentNode != nil {
            if value == currentNode.value {
                break
            } else if value < currentNode.value {
                parentNode = currentNode
                currentNode = currentNode.leftChild
            } else {
                parentNode = currentNode
                currentNode = currentNode.rightChild
            }
        }

        guard let nodeToDelete = currentNode else {
            throw MyError.invalidInput
        }

        // Case 1: Node to delete has no children
        if nodeToDelete.leftChild == nil && nodeToDelete.rightChild == nil {
            if parentNode == nil {
                rootNode = nil
            } else if parentNode?.leftChild === nodeToDelete {
                parentNode?.leftChild = nil
            } else {
                parentNode?.rightChild = nil
            }
        }

        // Case 2: Node to delete has one child
        else if nodeToDelete.leftChild != nil && nodeToDelete.rightChild == nil {
            if parentNode == nil {
                rootNode = nodeToDelete.leftChild
            } else if parentNode?.leftChild === nodeToDelete {
                parentNode?.leftChild = nodeToDelete.leftChild
            } else {
                parentNode?.rightChild = nodeToDelete.leftChild
            }
        } else if nodeToDelete.leftChild == nil && nodeToDelete.rightChild != nil {
            if parentNode == nil {
                rootNode = nodeToDelete.rightChild
            } else if parentNode?.leftChild === nodeToDelete {
                parentNode?.leftChild = nodeToDelete.rightChild
            } else {
                parentNode?.rightChild = nodeToDelete.rightChild
            }
        }

        // Case 3: Node to delete has two children
        else {
            var successorParentNode: Node<T>?
            var successorNode = nodeToDelete.rightChild

            while successorNode?.leftChild != nil {
                successorParentNode = successorNode
                successorNode = successorNode?.leftChild
            }

            nodeToDelete.value = successorNode!.value

            if successorParentNode == nil {
                nodeToDelete.rightChild = successorNode?.rightChild
            } else {
                successorParentNode?.leftChild = successorNode?.rightChild
            }
        }
    }

    // Function to find the minimum value in the binary search tree
    func findMin() -> T? {
        guard let root = rootNode else {
            return nil
        }

        var currentNode = root
        while currentNode.leftChild != nil {
            currentNode = currentNode.leftChild!
        }

        return currentNode.value
    }

    // Function to find the maximum value in the binary search tree
    func findMax() -> T? {
        guard let root = rootNode else {
            return nil
        }

        var currentNode = root
        while currentNode.rightChild != nil {
            currentNode = currentNode.rightChild!
        }

        return currentNode.value
    }

    // Function to check if the binary search tree is empty
    func isEmpty() -> Bool {
        return rootNode == nil
    }

    // Function to print the binary search tree in order (left-root-right)
    func printInOrder() {
        guard let root = rootNode else {
            return
        }

        printInOrder(node: root)
    }

    // Helper function to print the binary search tree in order recursively
    private func printInOrder(node: Node<T>) {
        if let leftChild = node.leftChild {
            printInOrder(node: leftChild)
        }

        print(node.value, terminator: " ")

        if let rightChild = node.rightChild {
            printInOrder