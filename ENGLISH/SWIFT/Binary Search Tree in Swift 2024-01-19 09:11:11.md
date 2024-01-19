```swift
class Node {
    var value: Int
    var left: Node?
    var right: Node?

    init(value: Int) {
        self.value = value
    }
}

class BinarySearchTree {
    var root: Node?

    func insert(value: Int) {
        if let root = root {
            _insert(value: value, currentNode: root)
        } else {
            root = Node(value: value)
        }
    }

    private func _insert(value: Int, currentNode: Node) {
        if value < currentNode.value {
            if let left = currentNode.left {
                _insert(value: value, currentNode: left)
            } else {
                currentNode.left = Node(value: value)
            }
        } else {
            if let right = currentNode.right {
                _insert(value: value, currentNode: right)
            } else {
                currentNode.right = Node(value: value)
            }
        }
    }

    func search(value: Int) -> Node? {
        if let root = root {
            return _search(value: value, currentNode: root)
        } else {
            return nil
        }
    }

    private func _search(value: Int, currentNode: Node) -> Node? {
        if value == currentNode.value {
            return currentNode
        } else if value < currentNode.value {
            if let left = currentNode.left {
                return _search(value: value, currentNode: left)
            } else {
                return nil
            }
        } else {
            if let right = currentNode.right {
                return _search(value: value, currentNode: right)
            } else {
                return nil
            }
        }
    }

    func delete(value: Int) {
        if let root = root {
            _delete(value: value, currentNode: root)
        }
    }

    private func _delete(value: Int, currentNode: Node) {
        if value == currentNode.value {
            if currentNode.left == nil && currentNode.right == nil {
                // Leaf node
                currentNode = nil
            } else if currentNode.left == nil {
                // Only right child
                currentNode = currentNode.right
            } else if currentNode.right == nil {
                // Only left child
                currentNode = currentNode.left
            } else {
                // Two children
                let successor = _findSuccessor(currentNode: currentNode)
                currentNode.value = successor.value
                _delete(value: successor.value, currentNode: successor)
            }
        } else if value < currentNode.value {
            if let left = currentNode.left {
                _delete(value: value, currentNode: left)
            }
        } else {
            if let right = currentNode.right {
                _delete(value: value, currentNode: right)
            }
        }
    }

    private func _findSuccessor(currentNode: Node) -> Node {
        var current = currentNode.right!
        while current.left != nil {
            current = current.left!
        }
        return current
    }

    func printTree() {
        _printTree(currentNode: root)
        print()
    }

    private func _printTree(currentNode: Node?) {
        if let currentNode = currentNode {
            _printTree(currentNode: currentNode.left)
            print(currentNode.value)
            _printTree(currentNode: currentNode.right)
        }
    }
}

let tree = BinarySearchTree()
tree.insert(value: 10)
tree.insert(value: 5)
tree.insert(value: 15)
tree.insert(value: 2)
tree.insert(value: 7)
tree.insert(value: 12)
tree.insert(value: 20)

tree.printTree()

if let node = tree.search(value: 15) {
    print("Found node with value 15: \(node.value)")
}

tree.delete(value: 15)

tree.printTree()
```

This code defines a binary search tree data structure in Swift. A binary search tree is a tree data structure where each node contains a value and two branches, one leading to the left subtree and one to the right subtree. The values in the left subtree are always less than the value of the parent node, and the values in the right subtree are always greater than the value of the parent node.

The BinarySearchTree class has a root property that stores the root node of the tree. The insert method is used to insert a new node into the tree. It takes a value as an argument and inserts it into the correct position in the tree. The search method is used to search for a node with a given value in the tree. It takes a value as an argument and returns the node if it is found, or nil if it is not found. The delete method is used to delete a node with a given value from the tree. It takes a value as an argument and deletes the node if it is found. The printTree method is used to print the values of the nodes in the tree in ascending order.

The _insert, _search, _delete, and _printTree methods are private helper methods that are used by the public methods.

The _findSuccessor method is used to find the successor of a node. The successor of a node is the node that would be visited next in an in-order traversal of the tree.

To use the binary search tree, you would first create an instance of the BinarySearchTree class. You can then use the insert method to insert values into the tree, the search method to search for values in the tree, the delete method to delete values from the tree, and the printTree method to print the values of the nodes in the tree in ascending order.