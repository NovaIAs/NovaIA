```c++
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

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

  // Insert value into the BST
  void insert(int val) {
    Node* new_node = new Node(val);
    insert_helper(new_node, root);
  }

  // Helper function for insert
  void insert_helper(Node* new_node, Node* curr_node) {
    if (root == nullptr) {
      root = new_node;
      return;
    }

    if (new_node->val < curr_node->val) {
      if (curr_node->left == nullptr) {
        curr_node->left = new_node;
        return;
      } else {
        insert_helper(new_node, curr_node->left);
      }
    } else {
      if (curr_node->right == nullptr) {
        curr_node->right = new_node;
        return;
      } else {
        insert_helper(new_node, curr_node->right);
      }
    }
  }

  // Search for value in the BST
  bool search(int val) {
    return search_helper(val, root);
  }

  // Helper function for search
  bool search_helper(int val, Node* curr_node) {
    if (curr_node == nullptr) {
      return false;
    }

    if (curr_node->val == val) {
      return true;
    } else if (val < curr_node->val) {
      return search_helper(val, curr_node->left);
    } else {
      return search_helper(val, curr_node->right);
    }
  }

  // Delete value from the BST
  void delete_val(int val) {
    root = delete_helper(val, root);
  }

  // Helper function for delete
  Node* delete_helper(int val, Node* curr_node) {
    if (curr_node == nullptr) {
      return nullptr;
    }

    if (val < curr_node->val) {
      curr_node->left = delete_helper(val, curr_node->left);
      return curr_node;
    } else if (val > curr_node->val) {
      curr_node->right = delete_helper(val, curr_node->right);
      return curr_node;
    } else {
      // Node to be deleted has been found

      // Case 1: Node has no children
      if (curr_node->left == nullptr && curr_node->right == nullptr) {
        delete curr_node;
        return nullptr;
      }
      // Case 2: Node has only one child
      else if (curr_node->left == nullptr) {
        Node* temp = curr_node->right;
        delete curr_node;
        return temp;
      } else if (curr_node->right == nullptr) {
        Node* temp = curr_node->left;
        delete curr_node;
        return temp;
      }
      // Case 3: Node has two children
      else {
        // Find the minimum value in the right subtree
        int min_val = find_min(curr_node->right);

        // Replace the current node's value with the minimum value
        curr_node->val = min_val;

        // Delete the minimum value from the right subtree
        curr_node->right = delete_helper(min_val, curr_node->right);

        return curr_node;
      }
    }
  }

  // Find the minimum value in the BST
  int find_min() {
    return find_min_helper(root);
  }

  // Helper function for find_min
  int find_min_helper(Node* curr_node) {
    if (curr_node->left == nullptr) {
      return curr_node->val;
    } else {
      return find_min_helper(curr_node->left);
    }
  }

  // Find the maximum value in the BST
  int find_max() {
    return find_max_helper(root);
  }

  // Helper function for find_max
  int find_max_helper(Node* curr_node) {
    if (curr_node->right == nullptr) {
      return curr_node->val;
    } else {
      return find_max_helper(curr_node->right);
    }
  }

  // Print the BST in order (left, root, right)
  void print_inorder() {
    print_inorder_helper(root);
  }

  // Helper function for print_inorder
  void print_inorder_helper(Node* curr_node) {
    if (curr_node == nullptr) {
      return;
    }

    print_inorder_helper(curr_node->left);
    cout << curr_node->val << " ";
    print_inorder_helper(curr_node->right);
  }

  // Print the BST in preorder (root, left, right)
  void print_preorder() {
    print_preorder_helper(root);
  }

  // Helper function for print_preorder
  void print_preorder_helper(Node* curr_node) {
    if (curr_node == nullptr) {
      return;
    }

    cout << curr_node->val << " ";
    print_preorder_helper(curr_node->left);
    print_preorder_helper(curr_node->right);
  }

  // Print the BST in postorder (left, right, root)
  void print_postorder() {
    print_postorder_helper(root);
  }

  // Helper function for print_postorder
  void print_postorder_helper(Node* curr_node) {
    if (curr_node == nullptr) {
      return;
    }

    print_postorder_helper(curr_node->left);
    print_postorder_helper(curr_node->right);
    cout << curr_node->val << " ";
  }
};

int main() {
  BinarySearchTree bst;
  bst.insert(5);
  bst.insert(3);
  bst.insert(7);
  bst.insert(2);
  bst.insert(4);
  bst.insert(6);
  bst.insert(8);

  cout << "Inorder: ";
  bst.print_inorder();
  cout << endl;

  cout << "Preorder: ";
  bst.print_preorder();
  cout << endl;

  cout << "Postorder: ";
  bst.print_postorder();
  cout << endl;

  cout << "Search for 5: " << bst.search(5) << endl;
  cout << "Search for 10: " << bst.search(10) << endl;

  bst.delete_val(5);
  cout << "Inorder after deleting 5: ";
  bst.print_inorder();
  cout << endl;

  cout << "Minimum value: " << bst.find_min() << endl;
  cout << "Maximum value: " << bst.find_max() << endl;

  return 0;
}
```

This code implements a binary search tree (BST) in C++. A BST is a data structure that organizes data in a way that allows for efficient searching, insertion, and deletion.

The code includes a `Node` class that represents a single node in the BST. Each node has a value, a pointer to the left child node, and a pointer to the right child node.

The `BinarySearchTree` class represents the entire BST. It includes a pointer to the root node of the BST and a number of methods for manipulating the BST, including:

* `insert`: Inserts a new value into the BST.
* `search`: Searches for a value in the BST.
* `delete_val`: Deletes a value from the BST.
* `find_min`: Finds the minimum value in the BST.
* `find_max`: Finds the maximum value in the BST.
* `print_inorder`: Prints the values in the BST in order (left, root, right).
* `print_preorder`: Prints the values in the BST in preorder (root, left, right).
* `print_postorder`: Prints the values in the BST in postorder (left, right, root).

The main function creates a BST and inserts a number of values into it. It then prints the BST in order, preorder, and postorder. It also searches for a value in the BST and deletes a value from the BST. Finally, it prints the minimum and maximum values in the BST.

This code is a good example of how to implement a BST in C++. It is also a good example of using a class to represent a data structure.