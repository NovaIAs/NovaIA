```c++
// Biblioteca estándar de C++
#include <iostream>
#include <vector>
#include <stack>
#include <queue>
#include <map>
#include <set>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace std;

// Clase para representar un nodo en un árbol binario
struct Node {
    int data;
    Node *left;
    Node *right;

    Node(int data) {
        this->data = data;
        this->left = nullptr;
        this->right = nullptr;
    }
};

// Función recursiva para insertar un nodo en un árbol binario
void insert(Node *&root, int data) {
    if (root == nullptr) {
        root = new Node(data);
    } else if (data < root->data) {
        insert(root->left, data);
    } else {
        insert(root->right, data);
    }
}

// Función recursiva para buscar un nodo en un árbol binario
bool search(Node *root, int data) {
    if (root == nullptr) {
        return false;
    } else if (root->data == data) {
        return true;
    } else if (data < root->data) {
        return search(root->left, data);
    } else {
        return search(root->right, data);
    }
}

// Función recursiva para obtener el nodo con el valor máximo en un árbol binario
Node *max(Node *root) {
    if (root == nullptr) {
        return nullptr;
    } else if (root->right == nullptr) {
        return root;
    } else {
        return max(root->right);
    }
}

// Función recursiva para obtener el nodo con el valor mínimo en un árbol binario
Node *min(Node *root) {
    if (root == nullptr) {
        return nullptr;
    } else if (root->left == nullptr) {
        return root;
    } else {
        return min(root->left);
    }
}

// Función recursiva para eliminar un nodo de un árbol binario
void remove(Node *&root, int data) {
    if (root == nullptr) {
        return;
    } else if (data < root->data) {
        remove(root->left, data);
    } else if (data > root->data) {
        remove(root->right, data);
    } else {
        if (root->left == nullptr) {
            Node *temp = root->right;
            delete root;
            root = temp;
        } else if (root->right == nullptr) {
            Node *temp = root->left;
            delete root;
            root = temp;
        } else {
            Node *temp = min(root->right);
            root->data = temp->data;
            remove(root->right, temp->data);
        }
    }
}

// Función recursiva para imprimir un árbol binario en orden
void inorder(Node *root) {
    if (root == nullptr) {
        return;
    }

    inorder(root->left);

    cout << root->data << " ";

    inorder(root->right);
}

// Función para imprimir un árbol binario en preorden
void preorder(Node *root) {
    if (root == nullptr) {
        return;
    }

    cout << root->data << " ";

    preorder(root->left);

    preorder(root->right);
}

// Función para imprimir un árbol binario en postorden
void postorder(Node *root) {
    if (root == nullptr) {
        return;
    }

    postorder(root->left);

    postorder(root->right);

    cout << root->data << " ";
}

// Función para imprimir un árbol binario por niveles
void levelorder(Node *root) {
    if (root == nullptr) {
        return;
    }

    queue<Node *> q;
    q.push(root);

    while (!q.empty()) {
        Node *current = q.front();
        q.pop();

        cout << current->data << " ";

        if (current->left != nullptr) {
            q.push(current->left);
        }

        if (current->right != nullptr) {
            q.push(current->right);
        }
    }
}

// Función para encontrar la altura de un árbol binario
int height(Node *root) {
    if (root == nullptr) {
        return 0;
    }

    int left_height = height(root->left