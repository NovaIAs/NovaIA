```d
import std.stdio, std.algorithm, std.range, std.typecons;

struct Node {
    int value;
    Node* left;
    Node* right;

    Node(int value) {
        this.value = value;
        this.left = null;
        this.right = null;
    }
};

Node* insert(Node* root, int value) {
    if (root == null) {
        return new Node(value);
    } else if (value < root.value) {
        root.left = insert(root.left, value);
    } else {
        root.right = insert(root.right, value);
    }
    return root;
}

Node* find(Node* root, int value) {
    if (root == null) {
        return null;
    } else if (value == root.value) {
        return root;
    } else if (value < root.value) {
        return find(root.left, value);
    } else {
        return find(root.right, value);
    }
}

Node* remove(Node* root, int value) {
    if (root == null) {
        return null;
    } else if (value < root.value) {
        root.left = remove(root.left, value);
    } else if (value > root.value) {
        root.right = remove(root.right, value);
    } else {
        if (root.left == null) {
            return root.right;
        } else if (root.right == null) {
            return root.left;
        } else {
            Node* min = root.right;
            while (min.left != null) {
                min = min.left;
            }
            root.value = min.value;
            root.right = remove(root.right, min.value);
        }
    }
    return root;
}

void inorder(Node* root) {
    if (root != null) {
        inorder(root.left);
        writeln(root.value);
        inorder(root.right);
    }
}

void preorder(Node* root) {
    if (root != null) {
        writeln(root.value);
        preorder(root.left);
        preorder(root.right);
    }
}

void postorder(Node* root) {
    if (root != null) {
        postorder(root.left);
        postorder(root.right);
        writeln(root.value);
    }
}

int main() {
    Node* root = null;
    root = insert(root, 10);
    root = insert(root, 5);
    root = insert(root, 15);
    root = insert(root, 2);
    root = insert(root, 7);
    root = insert(root, 12);
    root = insert(root, 20);

    writeln("Inorder:");
    inorder(root);

    writeln("Preorder:");
    preorder(root);

    writeln("Postorder:");
    postorder(root);

    writeln("Find 10:");
    Node* found = find(root, 10);
    if (found != null) {
        writeln("Found 10");
    } else {
        writeln("10 not found");
    }

    writeln("Remove 10:");
    root = remove(root, 10);

    writeln("Inorder after removing 10:");
    inorder(root);

    return 0;
}
```

This code implements a binary search tree in D.
A binary search tree is a data structure that stores data in a way that allows for efficient searching, insertion, and deletion.
The code uses a struct called Node to represent the nodes of the tree, and it includes functions to insert, find, and remove nodes from the tree, as well as functions to traverse the tree in different orders (inorder, preorder, and postorder).
The main function creates a binary search tree, inserts some data into it, and then searches for a value in the tree and removes it.
Finally, it prints the tree in inorder to show the result of the removal.