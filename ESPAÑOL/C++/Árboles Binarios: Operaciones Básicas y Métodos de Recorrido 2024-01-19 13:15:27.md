```c++
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cmath>
#include <iomanip>
#include <chrono>

using namespace std;
using namespace std::chrono;

// Definimos una estructura para almacenar los datos de cada nodo del árbol
struct Node {
  int value;
  Node* left;
  Node* right;
};

// Creamos una función para crear un nuevo nodo del árbol
Node* createNode(int value) {
  Node* newNode = new Node;
  newNode->value = value;
  newNode->left = nullptr;
  newNode->right = nullptr;
  return newNode;
}

// Creamos una función para insertar un nuevo nodo en el árbol
void insertNode(Node*& root, int value) {
  if (root == nullptr) {
    root = createNode(value);
    return;
  }
  if (value < root->value) {
    insertNode(root->left, value);
  } else {
    insertNode(root->right, value);
  }
}

// Creamos una función para encontrar un nodo en el árbol
Node* findNode(Node* root, int value) {
  if (root == nullptr) {
    return nullptr;
  }
  if (value == root->value) {
    return root;
  }
  if (value < root->value) {
    return findNode(root->left, value);
  } else {
    return findNode(root->right, value);
  }
}

// Creamos una función para eliminar un nodo del árbol
void deleteNode(Node*& root, int value) {
  if (root == nullptr) {
    return;
  }
  if (value == root->value) {
    if (root->left == nullptr && root->right == nullptr) {
      delete root;
      root = nullptr;
    } else if (root->left == nullptr) {
      Node* temp = root;
      root = root->right;
      delete temp;
    } else if (root->right == nullptr) {
      Node* temp = root;
      root = root->left;
      delete temp;
    } else {
      Node* temp = root->right;
      while (temp->left != nullptr) {
        temp = temp->left;
      }
      root->value = temp->value;
      deleteNode(root->right, temp->value);
    }
  } else if (value < root->value) {
    deleteNode(root->left, value);
  } else {
    deleteNode(root->right, value);
  }
}

// Creamos una función para imprimir el árbol en orden
void printTreeInOrder(Node* root) {
  if (root == nullptr) {
    return;
  }
  printTreeInOrder(root->left);
  cout << root->value << " ";
  printTreeInOrder(root->right);
}

// Creamos una función para imprimir el árbol en preorden
void printTreePreOrder(Node* root) {
  if (root == nullptr) {
    return;
  }
  cout << root->value << " ";
  printTreePreOrder(root->left);
  printTreePreOrder(root->right);
}

// Creamos una función para imprimir el árbol en postorden
void printTreePostOrder(Node* root) {
  if (root == nullptr) {
    return;
  }
  printTreePostOrder(root->left);
  printTreePostOrder(root->right);
  cout << root->value << " ";
}

// Creamos una función para calcular la altura del árbol
int treeHeight(Node* root) {
  if (root == nullptr) {
    return 0;
  }
  int leftHeight = treeHeight(root->left);
  int rightHeight = treeHeight(root->right);
  return max(leftHeight, rightHeight) + 1;
}

// Creamos una función para calcular el número de nodos del árbol
int treeSize(Node* root) {
  if (root == nullptr) {
    return 0;
  }
  int leftSize = treeSize(root->left);
  int rightSize = treeSize(root->right);
  return leftSize + rightSize + 1;
}

// Creamos una función para calcular el diámetro del árbol
int treeDiameter(Node* root) {
  if (root == nullptr) {
    return 0;
  }
  int leftDiameter = treeDiameter(root->left);
  int rightDiameter = treeDiameter(root->right);
  int leftHeight = treeHeight(root->left);
  int rightHeight = treeHeight(root->right);
  return max(leftDiameter, rightDiameter, leftHeight + rightHeight + 1);
}

// Creamos una función para calcular el ancho del árbol
int treeWidth(Node* root) {
  if (root == nullptr) {
    return 0;
  }
  int maxWidth = 0;
  queue<Node*> queue;
  queue.push(root);
  while (!queue.empty()) {
    int size = queue.size();
    int width = 0;
    for (int i = 0; i < size; i++) {
      Node* node = queue.front();
      queue.pop();
      width++;
      if (node->left != nullptr) {
        queue.push(node->left);
      }
      if (node->right != nullptr) {
        queue.push(node->right);