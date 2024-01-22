```c++
// Este programa implementa un árbol binario de búsqueda en C++.

#include <iostream>
#include <vector>

using namespace std;

// Clase para el nodo de un árbol binario de búsqueda.
class Node {
public:
    int value;
    Node *left;  // Puntero al hijo izquierdo.
    Node *right; // Puntero al hijo derecho.

    Node(int value) {
        this->value = value;
        left = nullptr;
        right = nullptr;
    }
};

// Clase para el árbol binario de búsqueda.
class BinarySearchTree {
public:
    Node *root; // Puntero a la raíz del árbol.

    BinarySearchTree() {
        root = nullptr;
    }

    // Agrega un nuevo nodo al árbol.
    void add(int value) {
        Node *new_node = new Node(value);
        if (root == nullptr) {
            root = new_node;
        } else {
            add_recursive(new_node, root);
        }
    }

    // Agrega un nuevo nodo al árbol de forma recursiva.
    void add_recursive(Node *new_node, Node *current_node) {
        if (new_node->value < current_node->value) {
            if (current_node->left == nullptr) {
                current_node->left = new_node;
            } else {
                add_recursive(new_node, current_node->left);
            }
        } else {
            if (current_node->right == nullptr) {
                current_node->right = new_node;
            } else {
                add_recursive(new_node, current_node->right);
            }
        }
    }

    // Busca un nodo en el árbol.
    Node *search(int value) {
        return search_recursive(value, root);
    }

    // Busca un nodo en el árbol de forma recursiva.
    Node *search_recursive(int value, Node *current_node) {
        if (current_node == nullptr) {
            return nullptr;
        }
        if (value == current_node->value) {
            return current_node;
        } else if (value < current_node->value) {
            return search_recursive(value, current_node->left);
        } else {
            return search_recursive(value, current_node->right);
        }
    }

    // Elimina un nodo del árbol.
    void remove(int value) {
        remove_recursive(value, root, nullptr);
    }

    // Elimina un nodo del árbol de forma recursiva.
    void remove_recursive(int value, Node *current_node, Node *parent_node) {
        if (current_node == nullptr) {
            return;
        }
        if (value == current_node->value) {
            // Si el nodo no tiene hijos, simplemente lo eliminamos.
            if (current_node->left == nullptr && current_node->right == nullptr) {
                if (parent_node == nullptr) {
                    root = nullptr;
                } else if (parent_node->left == current_node) {
                    parent_node->left = nullptr;
                } else {
                    parent_node->right = nullptr;
                }
                delete current_node;
            }
            // Si el nodo tiene un hijo, lo promovemos al lugar del nodo actual.
            else if (current_node->left == nullptr) {
                if (parent_node == nullptr) {
                    root = current_node->right;
                } else if (parent_node->left == current_node) {
                    parent_node->left = current_node->right;
                } else {
                    parent_node->right = current_node->right;
                }
                delete current_node;
            } else if (current_node->right == nullptr) {
                if (parent_node == nullptr) {
                    root = current_node->left;
                } else if (parent_node->left == current_node) {
                    parent_node->left = current_node->left;
                } else {
                    parent_node->right = current_node->left;
                }
                delete current_node;
            }
            // Si el nodo tiene dos hijos, lo reemplazamos con el nodo más pequeño de su subárbol derecho.
            else {
                Node *successor = current_node->right;
                while (successor->left != nullptr) {
                    successor = successor->left;
                }
                current_node->value = successor->value;
                remove_recursive(successor->value, current_node->right