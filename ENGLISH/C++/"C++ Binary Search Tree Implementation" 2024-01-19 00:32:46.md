```c++
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

class Node {
public:
    int val;
    Node* left;
    Node* right;

    Node(int val) {
        this->val = val;
        this->left = nullptr;
        this->right = nullptr;
    }
};

class BinarySearchTree {
public:
    Node* root;

    BinarySearchTree() {
        this->root = nullptr;
    }

    void insert(int val) {
        insertHelper(root, val);
    }

    void insertHelper(Node* node, int val) {
        if (node == nullptr) {
            root = new Node(val);
            return;
        }
        if (val < node->val) {
            insertHelper(node->left, val);
        } else {
            insertHelper(node->right, val);
        }
    }

    void delete(int val) {
        deleteHelper(root, val);
    }

    Node* deleteHelper(Node* node, int val) {
        if (node == nullptr) {
            return nullptr;
        }
        if (val < node->val) {
            node->left = deleteHelper(node->left, val);
        } else if (val > node->val) {
            node->right = deleteHelper(node->right, val);
        } else {
            if (node->left == nullptr) {
                return node->right;
            } else if (node->right == nullptr) {
                return node->left;
            } else {
                Node* minNode = findMin(node->right);
                node->val = minNode->val;
                node->right = deleteHelper(node->right, minNode->val);
            }
        }
        return node;
    }

    Node* findMin(Node* node) {
        if (node->left == nullptr) {
            return node;
        }
        return findMin(node->left);
    }

    Node* findMax(Node* node) {
        if (node->right == nullptr) {
            return node;
        }
        return findMax(node->right);
    }

    bool search(int val) {
        return searchHelper(root, val);
    }

    bool searchHelper(Node* node, int val) {
        if (node == nullptr) {
            return false;
        }
        if (val == node->val) {
            return true;
        } else if (val < node->val) {
            return searchHelper(node