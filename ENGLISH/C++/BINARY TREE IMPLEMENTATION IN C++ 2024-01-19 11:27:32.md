```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>

using namespace std;

class Node {
public:
    int data;
    Node *left;
    Node *right;

    Node(int data) {
        this->data = data;
        this->left = nullptr;
        this->right = nullptr;
    }
};

class BinaryTree {
public:
    Node *root;

    BinaryTree() {
        this->root = nullptr;
    }

    void insert(int data) {
        Node *new_node = new Node(data);
        if (this->root == nullptr) {
            this->root = new_node;
        } else {
            insert_helper(new_node, this->root);
        }
    }

    void insert_helper(Node *new_node, Node *current_node) {
        if (new_node->data < current_node->data) {
            if (current_node->left == nullptr) {
                current_node->left = new_node;
            } else {
                insert_helper(new_node, current_node->left);
            }
        } else {
            if (current_node->right == nullptr) {
                current_node->right = new_node;
            } else {
                insert_helper(new_node, current_node->right);
            }
        }
    }

    bool search(int data) {
        return search_helper(data, this->root);
    }

    bool search_helper(int data, Node *current_node) {
        if (current_node == nullptr) {
            return false;
        }

        if (current_node->data == data) {
            return true;
        }

        if (data < current_node->data) {
            return search_helper(data, current_node->left);
        } else {
            return search_helper(data, current_node->right);
        }
    }

    void delete_node(int data) {
        delete_node_helper(data, this->root);
    }

    Node *delete_node_helper(int data, Node *current_node) {
        if (current_node == nullptr) {
            return nullptr;
        }

        if (data < current_node->data) {
            current_node->left = delete_node_helper(data, current_node->left);
        } else if (data > current_node->data) {
            current_node->right = delete_node_helper(data, current_node->right);
        } else {
            if (current_node->left == nullptr) {
                Node *temp = current_node->right;
                delete current_node;
                return temp;
            } else if (current_node->right == nullptr) {
                Node *temp = current_node->left;
                delete current_node;
                return temp;
            } else {
                // find the minimum node in the right subtree
                Node *min_node = find_min_node(current_node->right);
                // copy the data of the minimum node to the current node
                current_node->data = min_node->data;
                // delete the minimum node from the right subtree
                current_node->right = delete_node_helper(min_node->data, current_node->right);
            }
        }

        return current_node;
    }

    Node *find_min_node(Node *node) {
        if (node->left == nullptr) {
            return node;
        } else {
            return find_min_node(node->left);
        }
    }

    void print_inorder() {
        print_inorder_helper(this->root);
        cout << endl;
    }

    void print_inorder_helper(Node *current_node) {
        if (current_node == nullptr) {
            return;
        }

        print_inorder_helper(current_node->left);
        cout << current_node->data << " ";
        print_inorder_helper(current_node->right);
    }

    void print_preorder() {
        print_preorder_helper(this->root);
        cout << endl;
    }

    void print_preorder_helper(Node *current_node) {
        if (current_node == nullptr) {
            return;
        }

        cout << current_node->data << " ";
        print_preorder_helper(current_node->left);
        print_preorder_helper(current_node->right);
    }

    void print_postorder() {
        print_postorder_helper(this->root);
        cout << endl;
    }

    void print_postorder_helper(Node *current_node) {
        if (current_node == nullptr) {
            return;
        }

        print_postorder_helper(current_node->left);
        print_postorder_helper(current_node->right);
        cout << current_node->data << " ";
    }

    int height() {
        return height_helper(this->root);
    }

    int height_helper(Node *current_node) {
        if (current_node == nullptr) {
            return 0;
        }

        int left_height = height_helper(current_node->left);
        int right_height = height_helper(current_node->right);

        return max(left_height, right_height) + 1;
    }

    int size() {
        return size_helper(this->root);
    }

    int size_helper(Node *current_node) {
        if (current_node == nullptr) {
            return 0;
        }

        return size_helper(current_node->left) + size_helper(current_node->right) + 1;
    }

    bool is_balanced() {
        return is_balanced_helper(this->root);
    }

    bool is_balanced_helper(Node *current_node) {
        if (current_node == nullptr) {
            return true;
        }

        int left_height = height_helper(current_node->left);
        int right_height = height_helper(current_node->right);

        return abs(left_height - right_height) <= 1 && is_balanced_helper(current_node->left) && is_balanced_helper(current_node->right);
    }

    void mirror_tree() {
        mirror_tree_helper(this->root);
    }

    void mirror_tree_helper(Node *current_node) {
        if (current_node == nullptr) {
            return;
        }

        Node *temp = current_node->left;
        current_node->left = current_node->right;
        current_node->right = temp;

        mirror_tree_helper(current_node->left);
        mirror_tree_helper(current_node->right);
    }
};

int main() {
    BinaryTree tree;

    tree.insert(50);
    tree.insert(30);
    tree.insert(20);
    tree.insert(40);
    tree.insert(70);
    tree.insert(60);
    tree.insert(80);

    tree.print_inorder(); // 20 30 40 50 60 70 80

    cout << "Is 40 present in the tree? " << (tree.search(40) ? "Yes" : "No") << endl; // Yes

    tree.delete_node(20);

    tree.print_inorder(); // 30 40 50 60 70 80

    cout << "Is 20 present in the tree? " << (tree.search(20) ? "Yes" : "No") << endl; // No

    cout << "Height of the tree: " << tree.height() << endl; // 3

    cout << "Size of the tree: " << tree.size() << endl; // 6

    cout << "Is the tree balanced? " << (tree.is_balanced() ? "Yes" : "No") << endl; // Yes

    cout << "Mirrored tree: ";
    tree.mirror_tree();
    tree.print_inorder(); // 80 70 60 50 40 30

    return 0;
}
```

This code implements a binary tree data structure in C++. A binary tree is a hierarchical data structure that consists of nodes, each of which can have a maximum of two child nodes. The nodes are organized in a parent-child relationship, where each node can have only one parent node but can have multiple child nodes. Binary trees are often used to represent hierarchical data, such as a file system or an organizational chart.

The code starts by defining the Node class, which represents a single node in the binary tree. The Node class has three member variables: data, left, and right. The data member variable stores the data associated with the node, while the left and right member variables store references to the left and right child nodes, respectively.

Next, the code defines the BinaryTree class, which represents a binary tree data structure. The BinaryTree class has a single member variable, root, which stores a reference to the root node of the tree.

The BinaryTree class has a number of methods that can be used to manipulate the tree. The insert method inserts a new node into the tree. The search method searches for a node with the given data value and returns true if the node is found and false otherwise. The delete_node method deletes a node from the tree. The print_inorder, print_preorder, and print_postorder methods print the tree in inorder, preorder, and postorder traversal, respectively. The height method returns the height of the tree, which is the maximum number of nodes along the longest path from the root node to a leaf node. The size method returns the number of nodes in the tree. The is_balanced method checks if the tree is balanced, which means that the heights of the left and right subtrees of each node differ by at most 1. The mirror_tree method mirrors the tree, which means that it swaps the left and right child nodes of each node.

Finally, the main function creates a binary tree and populates it with some data. It then calls the various methods of the BinaryTree class to manipulate and print the tree.