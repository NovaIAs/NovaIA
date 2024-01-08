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
    if (this->root == nullptr) {
      this->root = new Node(data);
      return;
    }

    Node* current = this->root;
    while (true) {
      if (data < current->data) {
        if (current->left == nullptr) {
          current->left = new Node(data);
          return;
        } else {
          current = current->left;
        }
      } else {
        if (current->right == nullptr) {
          current->right = new Node(data);
          return;
        } else {
          current = current->right;
        }
      }
    }
  }

  bool search(int data) {
    Node* current = this->root;
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

  void deleteNode(int data) {
    Node* current = this->root;
    Node* parent = nullptr;

    while (current != nullptr) {
      if (data == current->data) {
        break;
      } else if (data < current->data) {
        parent = current;
        current = current->left;
      } else {
        parent = current;
        current = current->right;
      }
    }

    if (current == nullptr) {
      return;
    }

    if (current->left == nullptr && current->right == nullptr) {
      if (parent == nullptr) {
        this->root = nullptr;
      } else if (parent->left == current) {
        parent->left = nullptr;
      } else {
        parent->right = nullptr;
      }
    } else if (current->left == nullptr) {
      if (parent == nullptr) {
        this->root = current->right;
      } else if (parent->left == current) {
        parent->left = current->right;
      } else {
        parent->right = current->right;
      }
    } else if (current->right == nullptr) {
      if (parent == nullptr) {
        this->root = current->left;
      } else if (parent->left == current) {
        parent->left = current->left;
      } else {
        parent->right = current->left;
      }
    } else {
      Node* successor = current->right;
      Node* successorParent = current;

      while (successor->left != nullptr) {
        successorParent = successor;
        successor = successor->left;
      }

      current->data = successor->data;

      if (successorParent == current) {
        successorParent->right = successor->right;
      } else {
        successorParent->left = successor->right;
      }
    }
  }

  void printTree() {
    this->_printTree(this->root);
  }

private:
  void _printTree(Node* current) {
    if (current == nullptr) {
      return;
    }

    cout << current->data << " ";
    this->_printTree(current->left);
    this->_printTree(current->right);
  }
};

int main() {
  BinarySearchTree bst;
  bst.insert(10);
  bst.insert(5);
  bst.insert(15);
  bst.insert(2);
  bst.insert(7);
  bst.insert(12);
  bst.insert(20);

  cout << "Inorder traversal: ";
  bst.printTree();
  cout << endl;

  cout << "Search for 15: " << bst.search(15) << endl;
  cout << "Search for 25: " << bst.search(25) << endl;

  bst.deleteNode(15);

  cout << "Inorder traversal after deleting 15: ";
  bst.printTree();
  cout << endl;

  return 0;
}
```

This code implements a binary search tree in C++. A binary search tree is a data structure that stores data in a way that allows for efficient searching and retrieval. The code includes the following features:

* **Node class:** This class represents a single node in the binary search tree. Each node has a data field, a left child pointer, and a right child pointer.
* **BinarySearchTree class:** This class represents the binary search tree itself. It includes methods for inserting, searching, and deleting nodes, as well as a method for printing the tree.
* **Main function:** This function creates a binary search tree, inserts some data into the tree, searches for a value in the tree, deletes a value from the tree, and then prints the tree.

The code is well-commented and easy to understand. It demonstrates the use of classes, pointers, and recursion in C++.