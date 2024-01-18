```d
import std.stdio, std.string, std.algorithm, std.random, std.numeric, std.range, std.traits, std.math, core.sys;

class Node {
    int value;
    Node* left;
    Node* right;

    Node(int value) {
        this.value = value;
        this.left = null;
        this.right = null;
    }
}

enum InorderState {
    LEFT,
    VISIT,
    RIGHT
}

class Tree {
    Node* root;

    Tree() {
        this.root = null;
    }

    void insert(int value) {
        if (this.root == null) {
            this.root = new Node(value);
        } else {
            this.insertRecursive(value, this.root);
        }
    }

    void insertRecursive(int value, Node* node) {
        if (value < node.value) {
            if (node.left == null) {
                node.left = new Node(value);
            } else {
                this.insertRecursive(value, node.left);
            }
        } else {
            if (node.right == null) {
                node.right = new Node(value);
            } else {
                this.insertRecursive(value, node.right);
            }
        }
    }

    int height() {
        return this.heightRecursive(this.root);
    }

    int heightRecursive(Node* node) {
        if (node == null) {
            return 0;
        } else {
            int leftHeight = this.heightRecursive(node.left);
            int rightHeight = this.heightRecursive(node.right);
            return max(leftHeight, rightHeight) + 1;
        }
    }

    int size() {
        return this.sizeRecursive(this.root);
    }

    int sizeRecursive(Node* node) {
        if (node == null) {
            return 0;
        } else {
            return this.sizeRecursive(node.left) + this.sizeRecursive(node.right) + 1;
        }
    }

    void inorderTraversal() {
        this.inorderTraversalRecursive(this.root);
    }

    void inorderTraversalRecursive(Node* node) {
        if (node == null) {
            return;
        } else {
            this.inorderTraversalRecursive(node.left);
            writefln(node.value);
            this.inorderTraversalRecursive(node.right);
        }
    }

    Node* find(int value) {
        return this.findRecursive(value, this.root);
    }

    Node* findRecursive(int value, Node* node) {
        if (node == null) {
            return null;
        } else if (node.value == value) {
            return node;
        } else if (value < node.value) {
            return this.findRecursive(value, node.left);
        } else {
            return this.findRecursive(value, node.right);
        }
    }

    void remove(int value) {
        this.root = this.removeRecursive(value, this.root);
    }

    Node* removeRecursive(int value, Node* node) {
        if (node == null) {
            return null;
        } else if (value < node.value) {
            node.left = this.removeRecursive(value, node.left);
            return node;
        } else if (value > node.value) {
            node.right = this.removeRecursive(value, node.right);
            return node;
        } else {
            if (node.left == null) {
                return node.right;
            } else if (node.right == null) {
                return node.left;
            } else {
                Node* successor = this.findMin(node.right);
                node.value = successor.value;
                node.right = this.removeRecursive(successor.value, node.right);
                return node;
            }
        }