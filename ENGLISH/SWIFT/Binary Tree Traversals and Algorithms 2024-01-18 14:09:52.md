```swift
// Import Foundation and UIKit
import Foundation
import UIKit

// Define a custom data structure to represent a tree node
class TreeNode<T> {
    let value: T
    var leftChild: TreeNode?
    var rightChild: TreeNode?
    
    init(value: T) {
        self.value = value
    }
}

// Define a function to traverse a binary tree in preorder
func preorderTraversal<T>(root: TreeNode<T>?) -> [T] {
    var result: [T] = []
    
    guard let root = root else { return result }
    
    result.append(root.value)
    result.append(contentsOf: preorderTraversal(root: root.leftChild))
    result.append(contentsOf: preorderTraversal(root: root.rightChild))
    
    return result
}

// Define a function to traverse a binary tree in inorder
func inorderTraversal<T>(root: TreeNode<T>?) -> [T] {
    var result: [T] = []
    
    guard let root = root else { return result }
    
    result.append(contentsOf: inorderTraversal(root: root.leftChild))
    result.append(root.value)
    result.append(contentsOf: inorderTraversal(root: root.rightChild))
    
    return result
}

// Define a function to traverse a binary tree in postorder
func postorderTraversal<T>(root: TreeNode<T>?) -> [T] {
    var result: [T] = []
    
    guard let root = root else { return result }
    
    result.append(contentsOf: postorderTraversal(root: root.leftChild))
    result.append(contentsOf: postorderTraversal(root: root.rightChild))
    result.append(root.value)
    
    return result
}

// Define a function to construct a binary tree from an array of values
func constructBinaryTree<T>(values: [T]) -> TreeNode<T>? {
    guard !values.isEmpty else { return nil }
    
    let root = TreeNode(value: values[0])
    
    var currentIndex = 1
    
    func insertNode(node: TreeNode<T>, value: T) {
        if value < node.value {
            if let leftChild = node.leftChild {
                insertNode(node: leftChild, value: value)
            } else {
                node.leftChild = TreeNode(value: value)
            }
        } else {
            if let rightChild = node.rightChild {
                insertNode(node: rightChild, value: value)
            } else {
                node.rightChild = TreeNode(value: value)
            }
        }
    }
    
    while currentIndex < values.count {
        insertNode(node: root, value: values[currentIndex])
        currentIndex += 1
    }
    
    return root
}

// Define a function to check if a binary tree is balanced
func isBalanced<T>(root: TreeNode<T>?) -> Bool {
    guard let root = root else { return true }
    
    let leftHeight = height(root: root.leftChild)
    let rightHeight = height(root: root.rightChild)
    
    return abs(leftHeight - rightHeight) <= 1 &&
        isBalanced(root: root.leftChild) &&
        isBalanced(root: root.rightChild)
}

// Define a function to calculate the height of a binary tree
func height<T>(root: TreeNode<T>?) -> Int {
    guard let root = root else { return 0 }
    
    let leftHeight = height(root: root.leftChild)
    let rightHeight = height(root: root.rightChild)
    
    return max(leftHeight, rightHeight) + 1
}

// Define a function to find the lowest common ancestor of two nodes in a binary tree
func lowestCommonAncestor<T>(root: TreeNode<T>?, node1: TreeNode<T>, node2: TreeNode<T>) -> TreeNode<T>? {
    guard let root = root else { return nil }
    
    if root === node1 || root === node2 {
        return root
    }
    
    let leftLCA = lowestCommonAncestor(root: root.leftChild, node1: node1, node2: node2)
    let rightLCA = lowestCommonAncestor(root: root.rightChild, node1: node1, node2: node2)
    
    if leftLCA != nil && rightLCA != nil {
        return root
    } else if leftLCA != nil {
        return leftLCA
    } else {
        return rightLCA
    }
}

// Define a function to find the maximum path sum in a binary tree
func maxPathSum(root: TreeNode<Int>?) -> Int {
    guard let root = root else { return 0 }
    
    var maxSum = Int.min
    
    func maxPathSumHelper(node: TreeNode<Int>?) -> Int {
        guard let node = node else { return 0 }
        
        let leftSum = max(0, maxPathSumHelper(node: node.leftChild))
        let rightSum = max(0, maxPathSumHelper(node: node.rightChild))
        
        maxSum = max(maxSum, node.value + leftSum + rightSum)
        
        return node.value + max(leftSum, rightSum)
    }
    
    maxPathSumHelper(node: root)
    
    return maxSum
}

// Define a function to find the diameter of a binary tree
func diameterOfBinaryTree(root: TreeNode<Int>?) -> Int {
    guard let root = root else { return 0 }
    
    var diameter = 0
    
    func diameterHelper(node: TreeNode<Int>?) -> Int {
        guard let node = node else { return 0 }
        
        let leftDiameter = diameterHelper(node: node.leftChild)
        let rightDiameter = diameterHelper(node: node.rightChild)
        
        diameter = max(diameter, leftDiameter + rightDiameter + 1)
        
        return max(leftDiameter, rightDiameter) + 1
    }
    
    diameterHelper(node: root)
    
    return diameter - 1
}

// Define a function to find the level order traversal of a binary tree
func levelOrderTraversal<T>(root: TreeNode<T>?) -> [[T]] {
    guard let root = root else { return [] }
    
    var result: [[T]] = []
    var queue: [TreeNode<T>] = [root]
    
    while !queue.isEmpty {
        var level: [T] = []
        
        for _ in 0..<queue.count {
            let node = queue.removeFirst()
            level.append(node.value)
            
            if let leftChild = node.leftChild {
                queue.append(leftChild)
            }
            
            if let rightChild = node.rightChild {
                queue.append(rightChild)
            }
        }
        
        result.append(level)
    }
    
    return result
}

// Define a function to find the zigzag level order traversal of a binary tree
func zigzagLevelOrderTraversal<T>(root: TreeNode<T>?) -> [[T]] {
    guard let root = root else { return [] }
    
    var result: [[T]] = []
    var queue: [TreeNode<T>] = [root]
    var zigzag = false
    
    while !queue.isEmpty {
        var level: [T] = []
        
        for _ in 0..<queue.count {
            let node = queue.removeFirst()
            level.append(node.value)
            
            if let leftChild = node.leftChild {
                queue.append(leftChild)
            }
            
            if let rightChild = node.rightChild {
                queue.append(rightChild)
            }
        }
        
        if zigzag {
            level.reverse()
        }
        
        result.append(level)
        
        zigzag = !zigzag
    }
    
    return result
}

// Define a function to find the boundary of a binary tree
func boundaryOfBinaryTree<T>(root: TreeNode<T>?) -> [T] {
    guard let root = root else { return [] }
    
    var result: [T] = []
    
    result.append(root.value)
    
    func leftBoundary(node: TreeNode<T>?) {
        guard let node = node else { return }
        
        if node.leftChild != nil || node.rightChild != nil {
            result.append(node.value)
        }
        
        if node.leftChild != nil {
            leftBoundary(node: node.leftChild)
        } else if node.rightChild != nil {
            leftBoundary(node: node.rightChild)
        }
    }
    
    func rightBoundary(node: TreeNode<T>?) {
        guard let node = node else { return }
        
        if node.rightChild != nil || node.leftChild != nil {
            result.append(node.value)
        }
        
        if node.rightChild != nil {
            rightBoundary(node: node.rightChild)
        } else if node.leftChild != nil {
            rightBoundary(node: node.leftChild)
        }
    }
    
    func leafNodes(node: TreeNode<T>?) {
        guard let node = node else { return }
        
        if node.leftChild == nil && node.rightChild == nil {
            result.append(node.value)
        } else {
            leafNodes(node: