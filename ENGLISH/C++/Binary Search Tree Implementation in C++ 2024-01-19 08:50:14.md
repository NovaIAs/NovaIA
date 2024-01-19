```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

using namespace std;

class Node {
public:
    int data;
    Node* left;
    Node* right;

    Node(int data) {
        this->data = data;
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

    void insert(int data) {
        Node* new_node = new Node(data);
        if (root == nullptr) {
            root = new_node;
            return;
        }

        Node* current = root;
        while (true) {
            if (data < current->data) {
                if (current->left == nullptr) {
                    current->left = new_node;
                    return;
                } else {
                    current = current->left;
                }
            } else {
                if (current->right == nullptr) {
                    current->right = new_node;
                    return;
                } else {
                    current = current->right;
                }
            }
        }
    }

    bool search(int data) {
        Node* current = root;
        while (current != nullptr) {
            if (data == current->data) {
                return true;
            } else if (data < current->data) {
                current = current->left;
            } else {
                current = current->right;
            }
        }

        return false;
    }

    void inorder_traversal() {
        inorder_traversal_helper(root);
    }

    void inorder_traversal_helper(Node* node) {
        if (node == nullptr) {
            return;
        }

        inorder_traversal_helper(node->left);
        cout << node->data << " ";
        inorder_traversal_helper(node->right);
    }

    void preorder_traversal() {
        preorder_traversal_helper(root);
    }

    void preorder_traversal_helper(Node* node) {
        if (node == nullptr) {
            return;
        }

        cout << node->data << " ";
        preorder_traversal_helper(node->left);
        preorder_traversal_helper(node->right);
    }

    void postorder_traversal() {
        postorder_traversal_helper(root);
    }

    void postorder_traversal_helper(Node* node) {
        if (node == nullptr) {
            return;
        }

        postorder_traversal_helper(node->left);
        postorder_traversal_helper(node->right);
        cout << node->data << " ";
    }

    int height() {
        return height_helper(root);
    }

    int height_helper(Node* node) {
        if (node == nullptr) {
            return 0;
        }

        int left_height = height_helper(node->left);
        int right_height = height_helper(node->right);

        return max(left_height, right_height) + 1;
    }

    void level_order_traversal() {
        queue<Node*> queue;
        queue.push(root);

        while (!queue.empty()) {
            Node* current = queue.front();
            queue.pop();

            cout << current->data << " ";

            if (current->left != nullptr) {
                queue.push(current->left);
            }

            if (current->right != nullptr) {
                queue.push(current->right);
            }
        }
    }

    void mirror_tree() {
        mirror_tree_helper(root);
    }

    void mirror_tree_helper(Node* node) {
        if (node == nullptr) {
            return;
        }

        Node* temp = node->left;
        node->left = node->right;
        node->right = temp;

        mirror_tree_helper(node->left);
        mirror_tree_helper(node->right);
    }

    bool is_balanced() {
        return is_balanced_helper(root) != -1;
    }

    int is_balanced_helper(Node* node) {
        if (node == nullptr) {
            return 0;
        }

        int left_height = is_balanced_helper(node->left);
        int right_height = is_balanced_helper(node->right);

        if (left_height == -1 || right_height == -1) {
            return -1;
        }

        if (abs(left_height - right_height) > 1) {
            return -1;
        }

        return max(left_height, right_height) + 1;
    }

    void delete_tree() {
        delete_tree_helper(root);
    }

    void delete_tree_helper(Node* node) {
        if (node == nullptr) {
            return;
        }

        delete_tree_helper(node->left);
        delete_tree_helper(node->right);
        delete node;
    }

    int find_minimum() {
        return find_minimum_helper(root);
    }

    int find_minimum_helper(Node* node) {
        if (node == nullptr) {
            return INT_MAX;
        }

        int left_minimum = find_minimum_helper(node->left);
        int right_minimum = find_minimum_helper(node->right);

        return min(node->data, min(left_minimum, right_minimum));
    }

    int find_maximum() {
        return find_maximum_helper(root);
    }

    int find_maximum_helper(Node* node) {
        if (node == nullptr) {
            return INT_MIN;
        }

        int left_maximum = find_maximum_helper(node->left);
        int right_maximum = find_maximum_helper(node->right);

        return max(node->data, max(left_maximum, right_maximum));
    }

    int find_lca(int n1, int n2) {
        return find_lca_helper(root, n1, n2);
    }

    int find_lca_helper(Node* node, int n1, int n2) {
        if (node == nullptr) {
            return -1;
        }

        if (node->data == n1 || node->data == n2) {
            return node->data;
        }

        int left_lca = find_lca_helper(node->left, n1, n2);
        int right_lca = find_lca_helper(node->right, n1, n2);

        if (left_lca != -1 && right_lca != -