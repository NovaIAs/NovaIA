// This code is a complex and differentiated implementation of a binary search tree in Swift, with additional features such as balancing and deletion.

class BinarySearchTree<T: Comparable> {
    
    private var root: Node<T>?
    
    // Insert a new value into the tree
    func insert(_ value: T) {
        root = insert(value, into: root)
    }
    
    // Recursive helper function for insertion
    private func insert(_ value: T, into node: Node<T>?) -> Node<T> {
        guard let node = node else {
            return Node(value: value)
        }
        
        if value < node.value {
            node.left = insert(value, into: node.left)
        } else {
            node.right = insert(value, into: node.right)
        }
        
        return balance(node)
    }
    
    // Search for a value in the tree
    func search(_ value: T) -> Node<T>? {
        return search(value, in: root)
    }
    
    // Recursive helper function for searching
    private func search(_ value: T, in node: Node<T>?) -> Node<T>? {
        guard let node = node else {
            return nil
        }
        
        if value == node.value {
            return node
        } else if value < node.value {
            return search(value, in: node.left)
        } else {
            return search(value, in: node.right)
        }
    }
    
    // Delete a value from the tree
    func delete(_ value: T) {
        root = delete(value, from: root)
    }
    
    // Recursive helper function for deletion
    private func delete(_ value: T, from node: Node<T>?) -> Node<T>? {
        guard let node = node else {
            return nil
        }
        
        if value == node.value {
            if node.left == nil {
                return node.right
            } else if node.right == nil {
                return node.left
            } else {
                let successor = findMin(in: node.right)!
                node.value = successor.value
                node.right = delete(successor.value, from: node.right)
            }
        } else if value < node.value {
            node.left = delete(value, from: node.left)
        } else {
            node.right = delete(value, from: node.right)
        }
        
        return balance(node)
    }
    
    // Find the minimum value in the tree
    func findMin() -> T? {
        return findMin(in: root)?.value
    }
    
    // Recursive helper function for finding the minimum value
    private func findMin(in node: Node<T>?) -> Node<T>? {
        guard let node = node else {
            return nil
        }
        
        if node.left == nil {
            return node
        } else {
            return findMin(in: node.left)
        }
    }
    
    // Find the maximum value in the tree
    func findMax() -> T? {
        return findMax(in: root)?.value
    }
    
    // Recursive helper function for finding the maximum value
    private func findMax(in node: Node<T>?) -> Node<T>? {
        guard let node = node else {
            return nil
        }
        
        if node.right == nil {
            return node
        } else {
            return findMax(in: node.right)
        }
    }
    
    // Balance the tree after an insertion or deletion
    private func balance(_ node: Node<T>) -> Node<T> {
        let balanceFactor = height(of: node.left) - height(of: node.right)
        
        if balanceFactor > 1 {
            if height(of: node.left?.left) >= height(of: node.left?.right) {
                return rightRotate(node)
            } else {
                node.left = leftRotate(node.left)
                return rightRotate(node)
            }
        } else if balanceFactor < -1 {
            if height(of: node.right?.right) >= height(of: node.right?.left) {
                return leftRotate(node)
            } else {
                node.right = rightRotate(node.right)
                return leftRotate(node)
            }
        }
        
        return node
    }
    
    // Left rotation of a node
    private func leftRotate(_ node: Node<T>) -> Node<T> {
        let rightChild = node.right
        node.right = rightChild?.left
        rightChild?.left = node
        
        return rightChild!
    }
    
    // Right rotation of a node
    private func rightRotate(_ node: Node<T>) -> Node<T> {
        let leftChild = node.left
        node.left = leftChild?.right
        leftChild?.right = node
        
        return leftChild!
    }
    
    // Calculate the height of a node
    private func height(of node: Node<T>?) -> Int {
        guard let node = node else {
            return 0
        }
        
        return 1 + max(height(of: node.left), height(of: node.right))
    }
    
    // Inner Node class
    private class Node<T: Comparable> {
        var value: T
        var left: Node<T>?
        var right: Node<T>?
        
        init(value: T) {
            self.value = value
        }
    }
}

// Example usage:
let tree = BinarySearchTree<Int>()

tree.insert(10)
tree.insert(5)
tree.insert(15)
tree.insert(2)
tree.insert(7)
tree.insert(12)
tree.insert(20)

if let node = tree.search(10) {
    print("Found node with value \(node.value)")
} else {
    print("Node not found")
}

tree.delete(15)

if let min = tree.findMin() {
    print("Minimum value in the tree is \(min)")
}

if let max = tree.findMax() {
    print("Maximum value in the tree is \(max)")
}