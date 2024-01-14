```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

using namespace std;

// Template for a generic binary tree node
template <typename T>
struct Node {
    T data;
    Node<T> *left;
    Node<T> *right;

    Node(const T& data) : data(data), left(nullptr), right(nullptr) {}
};

// Template for a binary search tree
template <typename T>
class BinarySearchTree {
public:
    // Insert a new node with the given data into the tree
    void insert(const T& data) {
        insert_helper(root, data);
    }

    // Search for a node with the given data in the tree
    bool search(const T& data) {
        return search_helper(root, data);
    }

    // Delete a node with the given data from the tree
    void delete_node(const T& data) {
        delete_node_helper(root, data);
    }

    // Print the tree in preorder traversal
    void print_preorder() {
        print_preorder_helper(root);
        cout << endl;
    }

    // Print the tree in inorder traversal
    void print_inorder() {
        print_inorder_helper(root);
        cout << endl;
    }

    // Print the tree in postorder traversal
    void print_postorder() {
        print_postorder_helper(root);
        cout << endl;
    }

    // Return the height of the tree
    int height() {
        return height_helper(root);
    }

    // Return the size of the tree
    int size() {
        return size_helper(root);
    }

private:
    Node<T> *root;

    // Helper function to insert a new node with the given data into the tree
    void insert_helper(Node<T> *&node, const T& data) {
        if (node == nullptr) {
            node = new Node<T>(data);
        } else if (data < node->data) {
            insert_helper(node->left, data);
        } else {
            insert_helper(node->right, data);
        }
    }

    // Helper function to search for a node with the given data in the tree
    bool search_helper(Node<T> *node, const T& data) {
        if (node == nullptr) {
            return false;
        } else if (data == node->data) {
            return true;
        } else if (data < node->data) {
            return search_helper(node->left, data);
        } else {
            return search_helper(node->right, data);
        }
    }

    // Helper function to delete a node with the given data from the tree
    void delete_node_helper(Node<T> *&node, const T& data) {
        if (node == nullptr) {
            return;
        } else if (data == node->data) {
            // Node to be deleted has no children
            if (node->left == nullptr && node->right == nullptr) {
                delete node;
                node = nullptr;
            }
            // Node to be deleted has only one child
            else if (node->left == nullptr) {
                Node<T> *temp = node;
                node = node->right;
                delete temp;
            } else if (node->right == nullptr) {
                Node<T> *temp = node;
                node = node->left;
                delete temp;
            }
            // Node to be deleted has two children
            else {
                // Find the smallest node in the right subtree
                Node<T> *temp = node->right;
                while (temp->left != nullptr) {
                    temp = temp->left;
                }

                // Copy the data from the smallest node to the node to be deleted
                node->data = temp->data;

                // Delete the smallest node
                delete_node_helper(node->right, temp->data);
            }
        } else if (data < node->data) {
            delete_node_helper(node->left, data);
        } else {
            delete_node_helper(node->right, data);
        }
    }

    // Helper function to print the tree in preorder traversal
    void print_preorder_helper(Node<T> *node) {
        if (node == nullptr) {
            return;
        }

        cout << node->data << " ";
        print_preorder_helper(node->left);
        print_preorder_helper(node->right);
    }

    // Helper function to print the tree in inorder traversal
    void print_inorder_helper(Node<T> *node) {
        if (node == nullptr) {
            return;
        }

        print_inorder_helper(node->left);
        cout << node->data << " ";
        print_inorder_helper(node->right);
    }

    // Helper function to print the tree in postorder traversal
    void print_postorder_helper(Node<T> *node) {
        if (node == nullptr) {
            return;
        }

        print_postorder_helper(node->left);
        print_postorder_helper(node->right);
        cout << node->data << " ";
    }

    // Helper function to return the height of the tree
    int height_helper(Node<T> *node) {
        if (node == nullptr) {
            return 0;
        }

        int left_height = height_helper(node->left);
        int right_height = height_helper(node->right);

        return max(left_height, right_height) + 1;
    }

    // Helper function to return the size of the tree
    int size_helper(Node<T> *node) {
        if (node == nullptr) {
            return 0;
        }

        int left_size = size_helper(node->left);
        int right_size = size_helper(node->right);

        return left_size + right_size + 1;
    }
};

// Template for a red-black tree node
template <typename T>
struct RBNode {
    T data;
    RBNode<T> *left;
    RBNode<T> *right;
    bool color; // True for red, false for black

    RBNode(const T& data) : data(data), left(nullptr), right(nullptr), color(true) {}
};

// Template for a red-black tree
template <typename T>
class RedBlackTree {
public:
    // Insert a new node with the given data into the tree
    void insert(const T& data) {
        insert_helper(root, data);
        fix_insert(data);
    }

    // Search for a node with the given data in the tree