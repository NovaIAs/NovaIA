```d
import std.stdio, std.algorithm, std.range, std.traits;

struct BinTree {
    int key;
    BinTree* left;
    BinTree* right;

    BinTree(int key) {
        this.key = key;
        this.left = null;
        this.right = null;
    }
}

void insert(BinTree*& root, int key) {
    if (root == null) {
        root = new BinTree(key);
    } else if (key < root->key) {
        insert(root->left, key);
    } else {
        insert(root->right, key);
    }
}

bool find(BinTree* root, int key) {
    if (root == null) {
        return false;
    } else if (key == root->key) {
        return true;
    } else if (key < root->key) {
        return find(root->left, key);
    } else {
        return find(root->right, key);
    }
}

void printTree(BinTree* root) {
    if (root == null) {
        return;
    }

    printTree(root->left);
    std.writefln("%d", root->key);
    printTree(root->right);
}

void deleteTree(BinTree*& root) {
    if (root == null) {
        return;
    }

    deleteTree(root->left);
    deleteTree(root->right);
    delete root;
    root = null;
}

int main() {
    BinTree* root = null;

    insert(root, 10);
    insert(root, 5);
    insert(root, 15);
    insert(root, 2);
    insert(root, 7);
    insert(root, 12);
    insert(root, 20);

    std.writefln("Binary Tree:");
    printTree(root);

    std.writefln("Find 15: %s", find(root, 15) ? "true" : "false");
    std.writefln("Find 42: %s", find(root, 42) ? "true" : "false");

    deleteTree(root);

    return 0;
}
```

This code defines a binary search tree data structure in the D programming language. It provides functions to insert, find, print, and delete nodes in the tree. The main function creates a binary search tree, inserts some values into it, and then searches for a specific value. Finally, it prints the tree and deletes it.

Here's a breakdown of the code:

1. **BinTree Struct**: This struct defines the binary tree node, which consists of an integer key and pointers to the left and right child nodes.

2. **insert Function**: This function inserts a new node with the given key into the binary search tree. It recursively traverses the tree, comparing the key to the current node's key, and inserts the new node as the left or right child accordingly.

3. **find Function**: This function searches for a node with the given key in the binary search tree. It recursively traverses the tree, comparing the key to the current node's key, and returns true if the node is found.

4. **printTree Function**: This function prints the binary search tree in an in-order traversal, which visits the left subtree, the current node, and then the right subtree.

5. **deleteTree Function**: This function deletes the binary search tree by recursively deleting the left and right subtrees and then deleting the current node.

6. **main Function**: The main function of the program creates a binary search tree, inserts some values into it, and then searches for a specific value. Finally, it prints the tree and deletes it.

This code demonstrates the implementation of a binary search tree in D, which is a versatile data structure used for efficient searching and organizing data.