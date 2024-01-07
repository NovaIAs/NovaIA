```swift
// MARK: - Data Structures

// Define a custom Node struct to represent each node in a binary tree
struct Node<T> {
    var value: T
    var leftChild: Node<T>?
    var rightChild: Node<T>?

    init(value: T) {
        self.value = value
        self.leftChild = nil
        self.rightChild = nil
    }
}

// Define a custom BinaryTree struct to represent the entire binary tree
struct BinaryTree<T> {
    var root: Node<T>?

    // Insert a new value into the binary tree using recursion
    mutating func insert(_ value: T) {
        // If the tree is empty, create a new root node
        guard let root = root else {
            self.root = Node(value: value)
            return
        }

        // Recursively insert the new value into the left or right subtree
        if value < root.value {
            root.leftChild?.insert(value) ?? (root.leftChild = Node(value: value))
        } else {
            root.rightChild?.insert(value) ?? (root.rightChild = Node(value: value))
        }
    }

    // Search for a value in the binary tree using recursion
    func search(_ value: T) -> Bool {
        // If the tree is empty, return false
        guard let root = root else {
            return false
        }

        // If the current node's value matches the search value, return true
        if root.value == value {
            return true
        }

        // Recursively search for the value in the left or right subtree
        if value < root.value {
            return root.leftChild?.search(value) ?? false
        } else {
            return root.rightChild?.search(value) ?? false
        }
    }

    // Perform a depth-first traversal (preorder, inorder, or postorder) on the binary tree
    func traverse(_ order: TraversalOrder, action: (T) -> Void) {
        switch order {
        case .preorder:
            preorderTraversal(root, action: action)
        case .inorder:
            inorderTraversal(root, action: action)
        case .postorder:
            postorderTraversal(root, action: action)
        }
    }

    // Perform a preorder traversal on the binary tree
    private func preorderTraversal(_ node: Node<T>?, action: (T) -> Void) {
        guard let node = node else {
            return
        }

        // Visit the current node
        action(node.value)

        // Recursively traverse the left and right subtrees
        preorderTraversal(node.leftChild, action: action)
        preorderTraversal(node.rightChild, action: action)
    }

    // Perform an inorder traversal on the binary tree
    private func inorderTraversal(_ node: Node<T>?, action: (T) -> Void) {
        guard let node = node else {
            return
        }

        // Recursively traverse the left subtree
        inorderTraversal(node.leftChild, action: action)

        // Visit the current node
        action(node.value)

        // Recursively traverse the right subtree
        inorderTraversal(node.rightChild, action: action)
    }

    // Perform a postorder traversal on the binary tree
    private func postorderTraversal(_ node: Node<T>?, action: (T) -> Void) {
        guard let node = node else {
            return
        }

        // Recursively traverse the left and right subtrees
        postorderTraversal(node.leftChild, action: action)
        postorderTraversal(node.rightChild, action: action)

        // Visit the current node
        action(node.value)
    }
}

// Define the TraversalOrder enum to represent the different traversal orders
enum TraversalOrder {
    case preorder
    case inorder
    case postorder
}

// MARK: - Example Usage

// Create a binary tree
var binaryTree = BinaryTree<Int>()

// Insert some values into the binary tree
binaryTree.insert(50)
binaryTree.insert(30)
binaryTree.insert(70)
binaryTree.insert(20)
binaryTree.insert(40)
binaryTree.insert(60)
binaryTree.insert(80)

// Search for a value in the binary tree
let searchResult = binaryTree.search(40)
print("Search result for 40:", searchResult)

// Perform a preorder traversal on the binary tree
print("Preorder traversal:")
binaryTree.traverse(.preorder) { value in
    print(value)
}

// Perform an inorder traversal on the binary tree
print("Inorder traversal:")
binaryTree.traverse(.inorder) { value in
    print(value)
}

// Perform a postorder traversal on the binary tree
print("Postorder traversal:")
binaryTree.traverse(.postorder) { value in
    print(value)
}
```

**Explanation:**

This code demonstrates the implementation of a binary tree data structure and its various operations in Swift.

1. **Node Struct:**
   - The `Node<T>` struct represents a single node in the binary tree. It has three properties: `value`, `leftChild`, and `rightChild`.

2. **BinaryTree Struct:**
   - The `BinaryTree<T>` struct represents the entire binary tree. It has one property: `root`, which points to the root node of the tree.

3. **Insertion:**
   - The `insert` method is used to insert a new value into the binary tree. It uses recursion to find the appropriate position for the new node based on its value.

4. **Searching:**
   - The `search` method is used to search for a value in the binary tree. It also uses recursion to traverse the tree and determine if the value exists.

5. **Traversal:**
   - The `traverse` method is used to perform a depth-first traversal (preorder, inorder, or postorder) on the binary tree. It takes a `TraversalOrder` enum value and a closure as arguments.

6. **TraversalOrder Enum:**
   - The `TraversalOrder` enum defines the different traversal orders that can be performed on the binary tree: preorder, inorder, and postorder.

7. **Example Usage:**
   - The code includes an example of creating a binary tree, inserting values into it, searching for a value, and performing different traversals on the tree.

This code provides a comprehensive implementation of a binary tree in Swift, allowing you to work with hierarchical data structures efficiently.