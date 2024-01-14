```d
import std.stdio, std.range, std.immutable, std.algorithm, std.container, std.concurrency, std.thread, std.exception;

// Define a generic binary tree node
class BinaryTreeNode!T {
    T value;
    immutable BinaryTreeNode!T? left;
    immutable BinaryTreeNode!T? right;
    immutable BinaryTreeNode!T? parent;

    // Constructor
    this(T value, BinaryTreeNode!T? left = null, BinaryTreeNode!T? right = null, BinaryTreeNode!T? parent = null) pure nothrow {
        this.value = value;
        this.left = left;
        this.right = right;
        this.parent = parent;
    }
}

// Define a binary search tree
class BinarySearchTree!T {
    immutable BinaryTreeNode!T? root;
    immutable int size;

    // Constructor
    this() pure nothrow {
        this.root = null;
        this.size = 0;
    }

    // Insert a value into the tree
    void insert(T value) pure nothrow {
        // Create a new node with the given value
        auto node = new BinaryTreeNode!T(value);

        // If the tree is empty, set the new node as the root
        if (this.root == null) {
            this.root = node;
        } else {
            // Find the correct position for the new node using a recursive helper function
            insertHelper(this.root, node);
        }

        // Increment the size of the tree
        this.size++;
    }

    // Helper function to insert a node into the tree
    void insertHelper(BinaryTreeNode!T? current, BinaryTreeNode!T? node) pure nothrow {
        // If the current node is null, we have found the correct position for the new node
        if (current == null) {
            current = node;
        } else if (node.value < current.value) {
            // If the new node's value is less than the current node's value, insert it into the left subtree
            insertHelper(current.left, node);
        } else {
            // If the new node's value is greater than or equal to the current node's value, insert it into the right subtree
            insertHelper(current.right, node);
        }
    }

    // Search for a value in the tree
    bool find(T value) pure nothrow {
        // Start searching from the root node
        return findHelper(this.root, value);
    }

    // Helper function to search for a value in the tree
    bool findHelper(BinaryTreeNode!T? current, T value) pure nothrow {
        // If the current node is null, the value is not in the tree
        if (current == null) {
            return false;
        } else if (current.value == value) {
            // If the current node's value is equal to the given value, the value is found
            return true;
        } else if (value < current.value) {
            // If the given value is less than the current node's value, search the left subtree
            return findHelper(current.left, value);
        } else {
            // If the given value is greater than or equal to the current node's value, search the right subtree
            return findHelper(current.right, value);
        }
    }

    // Delete a value from the tree
    void delete(T value) pure nothrow {
        // Find the node to be deleted
        auto nodeToDelete = findNode(value);

        // If the node is not found, return
        if (nodeToDelete == null) {
            return;
        }

        // Decrement the size of the tree
        this.size--;

        // Delete the node using a recursive helper function
        deleteHelper(nodeToDelete);
    }

    // Helper function to delete a node from the tree
    void deleteHelper(BinaryTreeNode!T? nodeToDelete) pure nothrow {
        // If the node to be deleted has no children, simply remove it
        if (nodeToDelete.left == null && nodeToDelete.right == null) {
            removeNode(nodeToDelete);
        } else if (nodeToDelete.left == null) {
            // If the node to be deleted has only a right child, replace it with the right child
            replaceNode(nodeToDelete, nodeToDelete.right);
        } else if (nodeToDelete.right == null) {
            // If the node to be deleted has only a left child, replace it with the left child
            replaceNode(nodeToDelete, nodeToDelete.left);
        } else {
            // If the node to be deleted has both a left and a right child, find the smallest node in the right subtree and replace the node to be deleted with that node
            auto successor = findMin(nodeToDelete.right);
            nodeToDelete.value = successor.value;
            deleteHelper(successor);
        }
    }

    // Helper function to find the smallest node in a subtree
    BinaryTreeNode!T? findMin(BinaryTreeNode!T? node) pure nothrow {
        // If the node has a left child, the smallest node is in the left subtree
        if (node.left != null) {
            return findMin(node.left);
        } else {
            // If the node has no left child, it is the smallest node in the subtree
            return node;
        }
    }

    // Helper function to find the node with the given value
    BinaryTreeNode!T? findNode(T value) pure nothrow {
        // Start searching from the root node
        return findNodeHelper(this.root, value);
    }

    // Helper function to find the node with the given value
    BinaryTreeNode!T? findNodeHelper(BinaryTreeNode!T? current, T value) pure nothrow {
        // If the current node is null, the value is not in the tree
        if (current == null) {
            return null;
        } else if (current.value == value) {
            // If the current node's value is equal to the given value, the value is found
            return current;
        } else if (value < current.value) {
            // If the given value is less than the current node's value, search the left subtree
            return findNodeHelper(current.left, value);
        } else {
            // If the given value is greater than or equal to the current node's value, search the right subtree
            return findNodeHelper(current.right, value);
        }
    }

    // Helper function to remove a node from the tree
    void removeNode(BinaryTreeNode!T? nodeToRemove) pure nothrow {
        // If the node to be removed is the root node, set the root to null
        if (nodeToRemove == this.root) {
            this.root = null;
        } else {
            // If the node to be removed is the left child of its parent, set the parent's left child to null
            if (nodeToRemove.parent.left == nodeToRemove) {
                nodeToRemove.parent.left = null;
            } else {
                // If the node to be removed is the right child of its parent, set the parent's right child to null
                nodeToRemove.parent.right = null;
            }
        }
    }

    // Helper function to replace a node with another node
    void replaceNode(BinaryTreeNode!T? nodeToReplace, BinaryTreeNode!T? newNode) pure nothrow {
        // If the node to be replaced is the root node, set the root to the new node
        if (nodeToReplace == this.root) {
            this.root = newNode;
        } else {
            // If the node to be replaced is the left child of its parent, set the parent's left child to the new node
            if (nodeToReplace.parent.left == nodeToReplace) {
                nodeToReplace.parent.left = newNode;
            } else {
                // If the node to be replaced is the right child of its parent, set the parent's right child to the new node
                nodeToReplace.parent.right = newNode;
            }
        }

        // Set the new node's parent to the parent of the node to be replaced
        newNode.parent = nodeToReplace.parent;
    }
}

// Define a function to print the values of a binary tree in order
void printTreeInOrder(BinarySearchTree!T tree) pure nothrow {
    // Use a recursive helper function to print the values of the tree in order
    printTreeInOrderHelper(tree.root);
}

// Helper function to print the values of a binary tree in order
void printTreeInOrderHelper(BinaryTreeNode!T? node) pure nothrow {
    // If the current node is null, return
    if (node == null) {
        return;
    }

    // Print the values of the left subtree
    printTreeInOrderHelper(node.left);

    // Print