```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

using namespace std;

// Clase que representa un nodo en un árbol binario
class Node {
public:
  // Valor almacenado en el nodo
  int value;

  // Punteros a los nodos hijo izquierdo y derecho
  Node* left;
  Node* right;

  // Constructor que toma un valor como argumento y lo almacena en el nodo
  Node(int value) {
    this->value = value;
    this->left = nullptr;
    this->right = nullptr;
  }
};

// Clase que representa un árbol binario de búsqueda
class BinarySearchTree {
public:
  // Puntero al nodo raíz del árbol
  Node* root;

  // Constructor que crea un árbol binario de búsqueda vacío
  BinarySearchTree() {
    this->root = nullptr;
  }

  // Función recursiva para insertar un valor en el árbol
  void insert(int value) {
    if (this->root == nullptr) {
      this->root = new Node(value);
    } else {
      insert_helper(this->root, value);
    }
  }

  // Función auxiliar para insertar un valor en el árbol
  void insert_helper(Node* node, int value) {
    if (value < node->value) {
      if (node->left == nullptr) {
        node->left = new Node(value);
      } else {
        insert_helper(node->left, value);
      }
    } else {
      if (node->right == nullptr) {
        node->right = new Node(value);
      } else {
        insert_helper(node->right, value);
      }
    }
  }

  // Función recursiva para buscar un valor en el árbol
  bool search(int value) {
    if (this->root == nullptr) {
      return false;
    } else {
      return search_helper(this->root, value);
    }
  }

  // Función auxiliar para buscar un valor en el árbol
  bool search_helper(Node* node, int value) {
    if (value == node->value) {
      return true;
    } else if (value < node->value) {
      if (node->left == nullptr) {
        return false;
      } else {
        return search_helper(node->left, value);
      }
    } else {
      if (node->right == nullptr) {
        return false;
      } else {
        return search_helper(node->right, value);
      }
    }
  }

  // Función para imprimir el árbol en orden
  void print_inorder() {
    print_inorder_helper(this->root);
  }

  // Función auxiliar para imprimir el árbol en orden
  void print_inorder_helper(Node* node) {
    if (node != nullptr) {
      print_inorder_helper(node->left);
      cout << node->value << " ";
      print_inorder_helper(node->right);
    }
  }
};

// Función principal
int main() {
  // Crear un árbol binario de búsqueda vacío
  BinarySearchTree tree;

  // Insertar algunos valores en el árbol
  tree.insert(10);
  tree.insert(5);
  tree.insert(15);
  tree.insert(2);
  tree.insert(7);
  tree.insert(12);
  tree.insert(20);

  // Buscar un valor en el árbol
  cout << "Buscar el valor 15: " << (tree.search(15) ? "Encontrado" : "No encontrado") << endl;

  // Imprimir el árbol en orden
  cout << "Imprimir el árbol en orden: ";
  tree.print_inorder();
  cout << endl;

  return 0;
}
```

Explicación del código:

* La clase `Node` representa un nodo en un árbol binario. Cada nodo tiene un valor almacenado, así como punteros a sus nodos hijo izquierdo y derecho.
* La clase `BinarySearchTree` representa un árbol binario de búsqueda. Tiene un puntero al nodo raíz del árbol, que es el nodo más alto del árbol.
* La función `insert()` se utiliza para insertar un valor en el árbol. Primero comprueba si el árbol está vacío. Si lo está, crea un nuevo nodo con el valor dado y lo establece como la raíz del árbol. Si el árbol no está vacío, llama a la función auxiliar `insert_helper()` para insertar el valor en el lugar correcto del árbol.
* La función `search()` se utiliza para buscar un valor en el árbol. De nuevo, primero comprueba si el árbol está vacío. Si lo está, devuelve `false`. Si el árbol no está vacío, llama a la función auxiliar `search_helper()` para buscar el valor en el árbol.
* La función `print_inorder()` se utiliza para imprimir el árbol en orden. Esto significa que visita cada nodo del árbol en orden ascendente. De nuevo, llama a la función auxiliar `print_inorder_helper()` para imprimir el árbol en orden.

Este código es un ejemplo de un árbol binario de búsqueda en C++. Puede utilizarse para almacenar y buscar datos de forma eficiente.