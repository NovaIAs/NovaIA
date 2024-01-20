```c++
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

// Clase para representar un nodo en un árbol binario de búsqueda
struct Node {
  int data;
  Node* left;
  Node* right;

  Node(int data) {
    this->data = data;
    this->left = NULL;
    this->right = NULL;
  }
};

// Clase para representar un árbol binario de búsqueda
class BinarySearchTree {
private:
  Node* root;

public:
  BinarySearchTree() {
    root = NULL;
  }

  void insert(int data) {
    // Si el árbol está vacío, insertamos el nuevo nodo como raíz
    if (root == NULL) {
      root = new Node(data);
      return;
    }

    // Si el árbol no está vacío, llamamos a la función recursiva para insertar el nuevo nodo
    insertRecursive(data, root);
  }

  void insertRecursive(int data, Node* current) {
    // Si el valor del nodo actual es mayor que el valor del nuevo nodo, llamamos a la función recursiva para insertar el nuevo nodo en el subárbol izquierdo
    if (current->data > data) {
      if (current->left == NULL) {
        current->left = new Node(data);
      } else {
        insertRecursive(data, current->left);
      }
    }

    // Si el valor del nodo actual es menor o igual que el valor del nuevo nodo, llamamos a la función recursiva para insertar el nuevo nodo en el subárbol derecho
    else {
      if (current->right == NULL) {
        current->right = new Node(data);
      } else {
        insertRecursive(data, current->right);
      }
    }
  }

  bool search(int data) {
    // Si el árbol está vacío, devolvemos falso
    if (root == NULL) {
      return false;
    }

    // Si el árbol no está vacío, llamamos a la función recursiva para buscar el nodo con el valor dado
    return searchRecursive(data, root);
  }

  bool searchRecursive(int data, Node* current) {
    // Si el valor del nodo actual es igual al valor del nodo que estamos buscando, devolvemos verdadero
    if (current->data == data) {
      return true;
    }

    // Si el valor del nodo actual es mayor que el valor del nodo que estamos buscando, llamamos a la función recursiva para buscar el nodo en el subárbol izquierdo
    if (current->data > data) {
      if (current->left == NULL) {
        return false;
      } else {
        return searchRecursive(data, current->left);
      }
    }

    // Si el valor del nodo actual es menor que el valor del nodo que estamos buscando, llamamos a la función recursiva para buscar el nodo en el subárbol derecho
    else {
      if (current->right == NULL) {
        return false;
      } else {
        return searchRecursive(data, current->right);
      }
    }
  }

  void deleteNode(int data) {
    // Si el árbol está vacío, no hacemos nada
    if (root == NULL) {
      return;
    }

    // Si el nodo que queremos eliminar es la raíz, llamamos a la función recursiva para eliminarlo
    if (root->data == data) {
      deleteRoot();
      return;
    }

    // Si el nodo que queremos eliminar no es la raíz, llamamos a la función recursiva para eliminarlo
    deleteNodeRecursive(data, root);
  }

  void deleteNodeRecursive(int data, Node* current) {
    // Si el valor del nodo actual es mayor que el valor del nodo que queremos eliminar, llamamos a la función recursiva para eliminar el nodo en el subárbol izquierdo
    if (current->data > data) {
      if (current->left == NULL) {
        return;
      }

      if (current->left->data == data) {
        deleteNodeFromLeft(current);
      } else {
        deleteNodeRecursive(data, current->left);
      }
    }

    // Si el valor del nodo actual es menor que el valor del nodo que queremos eliminar, llamamos a la función recursiva para eliminar el nodo en el subárbol derecho
    else {
      if (current->right == NULL) {
        return;
      }

      if (current->right->data == data) {
        deleteNodeFromRight(current);
      } else {
        deleteNodeRecursive(data, current->right);
      }
    }
  }

  void deleteRoot() {
    // Si el árbol tiene dos hijos, llamamos a la función para encontrar el nodo más pequeño en el subárbol derecho y lo copiamos en la raíz
    if (root->left != NULL && root->right != NULL) {
      Node* smallestNode = findSmallestNode(root->right);
      root->data = smallestNode->data;
      deleteNodeRecursive(smallestNode->data, root->right);
    }

    // Si el árbol tiene un solo hijo, lo hacemos la raíz
    else if (root->left != NULL) {
      root = root->left;
    } else if (root->right != NULL) {
      root = root->right;
    }

    // Si el árbol no tiene hijos, lo hacemos nulo
    else {
      root = NULL;
    }
  }

  void deleteNodeFromLeft(Node* parent