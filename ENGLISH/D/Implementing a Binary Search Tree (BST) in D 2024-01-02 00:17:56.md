Certainly! Here's a complex and differentiated code written in the D programming language. This code demonstrates the implementation of a Binary Search Tree (BST) data structure along with various operations such as insertion, deletion, and traversal.

```d
import std.stdio;
import std.algorithm.iteration;

// Node structure for Binary Search Tree
struct Node {
    int data;
    Node* left;
    Node* right;
}

// Function to create a new node
Node* createNode(int value) {
    auto node = new Node;
    node.data = value;
    node.left = node.right = null;
    return node;
}

// Function to insert a node in BST
Node* insert(Node* root, int value) {
    if (root is null) {
        return createNode(value);
    }

    if (value < root.data) {
        root.left = insert(root.left, value);
    }
    else if (value > root.data) {
        root.right = insert(root.right, value);
    }

    return root;
}

// Function to delete a node from BST
Node* deleteNode(Node* root, int value) {
    if (root is null) {
        return root;
    }

    if (value < root.data) {
        root.left = deleteNode(root.left, value);
    }
    else if (value > root.data) {
        root.right = deleteNode(root.right, value);
    }
    else {
        if (root.left is null) {
            auto temp = root.right;
            delete root;
            return temp;
        }
        else if (root.right is null) {
            auto temp = root.left;
            delete root;
            return temp;
        }

        auto temp = minValue(root.right);
        root.data = temp.data;
        root.right = deleteNode(root.right, temp.data);
    }

    return root;
}

// Function to find the minimum value in BST
Node* minValue(Node* node) {
    auto current = node;
    while (current.left !is null) {
        current = current.left;
    }
    return current;
}

// Function to perform inorder traversal of BST
void inorder(Node* root) {
    if (root is null) {
        return;
    }

    inorder(root.left);
    writeln(root.data);
    inorder(root.right);
}

void main() {
    Node* root = null;
    root = insert(root, 50);
    insert(root, 30);
    insert(root, 20);
    insert(root, 40);
    insert(root, 70);
    insert(root, 60);
    insert(root, 80);

    writeln("Binary Search Tree:");
    inorder(root);

    deleteNode(root, 20);
    deleteNode(root, 30);

    writeln("\nUpdated Binary Search Tree:");
    inorder(root);
}
```

This code implements a Binary Search Tree (BST), a data structure that allows efficient searching, insertion, and deletion operations. 

The `struct Node` represents a node in the BST and has three fields: `data` holds the integer value, `left` points to the left child, and `right` points to the right child.

The `createNode` function is used to create a new node with the given value.

The `insert` function inserts a node in the BST by recursively traversing the tree based on the comparison of the value with the current node's data.

The `deleteNode` function deletes a node from the BST by recursively searching for the node to be deleted and handling different scenarios based on the existence of child nodes.

The `minValue` function finds the minimum value in the BST by traversing to the leftmost node.

The `inorder` function performs an inorder traversal of the BST, printing the values in ascending order.

In the `main` function, we create a BST, insert some values, and then perform inorder traversal before and after deleting some nodes.

This code demonstrates the fundamental operations of a BST and can serve as a starting point for more complex applications involving binary search trees.