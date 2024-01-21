```c++
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

// Clase para representar un nodo en un árbol binario
template <typename T>
class Node {
public:
    T data;
    Node<T>* left;
    Node<T>* right;
    Node(T data) : data(data), left(nullptr), right(nullptr) {}
};

// Función para crear un árbol binario a partir de una lista de valores
template <typename T>
Node<T>* create_tree(const vector<T>& values) {
    if (values.empty()) {
        return nullptr;
    }
    Node<T>* root = new Node<T>(values[0]);
    for (size_t i = 1; i < values.size(); i++) {
        insert_node(root, values[i]);
    }
    return root;
}

// Función para insertar un nodo en un árbol binario
template <typename T>
void insert_node(Node<T>* root, T value) {
    if (value < root->data) {
        if (root->left == nullptr) {
            root->left = new Node<T>(value);
        } else {
            insert_node(root->left, value);
        }
    } else {
        if (root->right == nullptr) {
            root->right = new Node<T>(value);
        } else {
            insert_node(root->right, value);
        }
    }
}

// Función para encontrar el nodo más cercano a un valor dado en un árbol binario de búsqueda
template <typename T>
Node<T>* find_closest_node(Node<T>* root, T value) {
    Node<T>* closest = nullptr;
    int min_diff = INT_MAX;
    find_closest_node_recursive(root, value, closest, min_diff);
    return closest;
}

// Función auxiliar para encontrar el nodo más cercano a un valor dado en un árbol binario de búsqueda
template <typename T>
void find_closest_node_recursive(Node<T>* root, T value, Node<T>*& closest, int& min_diff) {
    if (root == nullptr) {
        return;
    }
    int diff = abs(root->data - value);
    if (diff < min_diff) {
        closest = root;
        min_diff = diff;
    }
    if (value < root->data) {
        find_closest_node_recursive(root->left, value, closest, min_diff);
    } else {
        find_closest_node_recursive(root->right, value, closest, min_diff);
    }
}

// Función para imprimir un árbol binario en forma de árbol
template <typename T>
void print_tree(Node<T>* root) {
    if (root == nullptr) {
        return;
    }
    cout << root->data << " ";
    print_tree(root->left);
    print_tree(root->right);
}

int main() {
    vector<int> values = {50, 30, 20, 40, 70, 60, 80};
    Node<int>* root = create_tree(values);

    int value = 35;
    Node<int>* closest = find_closest_node(root, value);
    cout << "El nodo más cercano a " << value << " es " << closest->data << endl;

    print_tree(root);
    cout << endl;

    return 0;
}
```

Este código crea un árbol binario a partir de una lista de valores, encuentra el nodo más cercano a un valor dado en el árbol y lo imprime.

El árbol binario es una estructura de datos en la que cada nodo tiene un valor y dos punteros, uno al nodo izquierdo y otro al nodo derecho. El árbol se construye insertando los valores en el orden correcto, de modo que el valor de cada nodo sea mayor que el valor de su nodo izquierdo y menor que el valor de su nodo derecho.

El código utiliza una función recursiva para encontrar el nodo más cercano a un valor dado en el árbol. La función empieza en el nodo raíz y compara el valor dado con el valor del nodo actual. Si el valor dado es menor que el valor del nodo actual, la función se llama recursivamente con el nodo izquierdo. Si el valor dado es mayor que el valor del nodo actual, la función se llama recursivamente con el nodo derecho.

La función también mantiene un seguimiento del nodo más cercano encontrado hasta el momento. Cuando la función encuentra un nodo que está más cerca del valor dado que el nodo más cercano anterior, actualiza el nodo más cercano y la diferencia mínima entre el valor dado y el valor del nodo más cercano.

El código también utiliza una función recursiva para imprimir el árbol en forma de árbol. La función empieza en el nodo raíz y visita primero el nodo izquierdo, luego el nodo derecho y finalmente el nodo actual.