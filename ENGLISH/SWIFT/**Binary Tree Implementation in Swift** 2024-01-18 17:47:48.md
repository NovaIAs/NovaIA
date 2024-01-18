```swift
// This code is a custom implementation of a binary tree in Swift.
// It includes methods for inserting, searching, and deleting nodes, as well as a method for printing the tree in a hierarchical format.

class BinaryTreeNode<T: Comparable> {
    // The value stored in this node.
    var value: T

    // The left child node.
    var left: BinaryTreeNode?

    // The right child node.
    var right: BinaryTreeNode?

    // Initializes a new binary tree node with the given value.
    init(value: T) {
        self.value = value
    }

    // Inserts a new node with the given value into the tree, maintaining its binary search tree properties.
    func insert(value: T) {
        if value < self.value {
            if let leftNode = self.left {
                leftNode.insert(value: value)
            } else {
                self.left = BinaryTreeNode(value: value)
            }
        } else {
            if let rightNode = self.right {
                rightNode.insert(value: value)
            } else {
                self.right = BinaryTreeNode(value: value)
            }
        }
    }

    // Searches for a node with the given value in the tree.
    // Returns the node if found, or nil if not found.
    func search(value: T) -> BinaryTreeNode<T>? {
        if value == self.value {
            return self
        } else if value < self.value {
            if let leftNode = self.left {
                return leftNode.search(value: value)
            }
        } else {
            if let rightNode = self.right {
                return rightNode.search(value: value)
            }
        }

        return nil
    }

    // Deletes a node with the given value from the tree, maintaining its binary search tree properties.
    func delete(value: T) {
        // Case 1: The node to be deleted is a leaf node.
        if self.left == nil && self.right == nil {
            // Simply remove the node from its parent.
            self.removeFromParent()
        }
        // Case 2: The node to be deleted has one child node.
        else if self.left != nil && self.right == nil {
            // Replace the node with its left child.
            self.replaceWith(node: self.left!)
        } else if self.left == nil && self.right != nil {
            // Replace the node with its right child.
            self.replaceWith(node: self.right!)
        }
        // Case 3: The node to be deleted has two child nodes.
        else {
            // Find the smallest node in the right subtree.
            var smallestNode = self.right!
            while smallestNode.left != nil {
                smallestNode = smallestNode.left!
            }

            // Replace the value of the node to be deleted with the value of the smallest node.
            self.value = smallestNode.value

            // Delete the smallest node.
            smallestNode.delete(value: smallestNode.value)
        }
    }

    // Removes the node from its parent.
    private func removeFromParent() {
        if let parent = self.parent {
            if self === parent.left {
                parent.left = nil
            } else {
                parent.right = nil
            }
        }
    }

    // Replaces the node with the given node.
    private func replaceWith(node: BinaryTreeNode<T>) {
        if let parent = self.parent {
            if self === parent.left {
                parent.left = node
            } else {
                parent.right = node
            }
        }

        node.parent = parent
        node.left = self.left
        node.right = self.right
    }

    // Returns the parent node of this node, or nil if this node is the root node.
    var parent: BinaryTreeNode<T>? {
        var node: BinaryTreeNode<T>? = self
        while node?.parent != nil {
            node = node?.parent
        }

        return node
    }

    // Prints the tree in a hierarchical format.
    func printTree() {
        print(self.value)

        if let leftNode = self.left {
            print("  Left:")
            leftNode.printTree()
        }

        if let rightNode = self.right {
            print("  Right:")
            rightNode.printTree()
        }
    }
}

// Example usage:
let rootNode = BinaryTreeNode(value: 10)
rootNode.insert(value: 5)
rootNode.insert(value: 15)
rootNode.insert(value: 2)
rootNode.insert(value: 7)
rootNode.insert(value: 12)
rootNode.insert(value: 20)

rootNode.printTree()

let foundNode = rootNode.search(value: 7)
if let foundNode = foundNode {
    print("Found node with value \(foundNode.value)")
} else {
    print("Node not found")
}

rootNode.delete(value: 15)

rootNode.printTree()
```

This code is a complete implementation of a binary tree in Swift. It includes methods for inserting, searching, and deleting nodes, as well as a method for printing the tree in a hierarchical format.

The code begins with the `BinaryTreeNode` class, which represents a single node in the tree. Each node has a value, a left child node, and a right child node.

The `insert` method inserts a new node with the given value into the tree. It does this by comparing the value to the value of the current node. If the value is less than the current node, the `insert` method calls itself on the left child node. If the value is greater than the current node, the `insert` method calls itself on the right child node.

The `search` method searches for a node with the given value in the tree. It does this by comparing the value to the value of the current node. If the value is equal to the current node, the `search` method returns the current node. If the value is less than the current node, the `search` method calls itself on the left child node. If the value is greater than the current node, the `search` method calls itself on the right child node.

The `delete` method deletes a node with the given value from the tree. It does this by first finding the node to be deleted. Once the node is found, the `delete` method checks if the node has any children. If the node has no children, the `delete` method simply removes the node from its parent. If the node has one child, the `delete` method replaces the node with its child. If the node has two children, the `delete` method finds the smallest node in the right subtree and replaces the node to be deleted with the smallest node.

The `removeFromParent` method removes the node from its parent. The `replaceWith` method replaces the node with the given node. The `parent` property returns the parent node of the current node.

The `printTree` method prints the tree in a hierarchical format. It does this by printing the value of the current node, and then recursively printing the left and right child nodes.

Finally, the code includes an example usage of the `BinaryTreeNode` class. It creates a binary tree, inserts some values into the tree, searches for a value in the tree, and deletes a value from the tree.