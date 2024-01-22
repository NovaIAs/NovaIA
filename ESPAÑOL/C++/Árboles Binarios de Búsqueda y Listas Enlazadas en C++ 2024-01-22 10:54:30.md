```c++
using namespace std;

// Clase de nodo para listas enlazadas
class Node {
public:
    int data;
    Node *next;

    // Constructor
    Node(int data) {
        this->data = data;
        this->next = nullptr;
    }
};

// Clase de lista enlazada
class LinkedList {
public:
    Node *head;

    // Constructor
    LinkedList() {
        this->head = nullptr;
    }

    // Añadir un nodo al final de la lista
    void addNode(int data) {
        Node *node = new Node(data);

        if (head == nullptr) {
            head = node;
        } else {
            Node *current = head;

            while (current->next != nullptr) {
                current = current->next;
            }

            current->next = node;
        }
    }

    // Eliminar un nodo de la lista por su valor
    void deleteNode(int data) {
        if (head == nullptr) {
            return;
        } else if (head->data == data) {
            head = head->next;
        } else {
            Node *current = head;
            Node *previous = nullptr;

            while (current != nullptr && current->data != data) {
                previous = current;
                current = current->next;
            }

            if (current != nullptr) {
                previous->next = current->next;
            }
        }
    }

    // Buscar un nodo en la lista por su valor
    Node *findNode(int data) {
        Node *current = head;

        while (current != nullptr && current->data != data) {
            current = current->next;
        }

        return current;
    }

    // Imprimir la lista
    void printList() {
        Node *current = head;

        while (current != nullptr) {
            cout << current->data << " ";
            current = current->next;
        }

        cout << endl;
    }
};

// Clase de árbol binario de búsqueda
class BinarySearchTree {
public:
    Node *root;

    // Constructor
    BinarySearchTree() {
        this->root = nullptr;
    }

    // Insertar un nodo en el árbol
    void insertNode(int data) {
        Node *node = new Node(data);

        if (root == nullptr) {
            root = node;
        } else {
            insertNodeHelper(node, root);
        }
    }

    // Insertar un nodo en el árbol recursivamente
    void insertNodeHelper(Node *node, Node *current) {
        if (node->data < current->data) {
            if (current->left == nullptr) {
                current->left = node;
            } else {
                insertNodeHelper(node, current->left);
            }
        } else if (node->data > current->data) {
            if (current->right == nullptr) {
                current->right = node;
            } else {
                insertNodeHelper(node, current->right);
            }
        }
    }

    // Buscar un nodo en el árbol por su valor
    Node *findNode(int data) {
        return findNodeHelper(data, root);
    }

    // Buscar un nodo en el árbol recursivamente
    Node *findNodeHelper(int data, Node *current) {
        if (current == nullptr) {
            return nullptr;
        } else if (data == current->data) {
            return current;
        } else if (data < current->data) {
            return findNodeHelper(data, current->left);
        } else {
            return findNodeHelper(data, current->right);
        }
    }

    // Eliminar un nodo del árbol por su valor
    void deleteNode(int data) {
        deleteNodeHelper(data, root, nullptr);
    }

    // Eliminar un nodo del árbol recursivamente
    void deleteNodeHelper(int data, Node *current, Node *parent) {
        if (current == nullptr) {
            return;
        } else if (data < current->data) {
            deleteNodeHelper(data, current->left, current);
        } else if (data > current->data) {
            deleteNodeHelper(data, current->right, current);
        } else {
            if (current->left == nullptr && current->right == nullptr) {
                if (parent == nullptr) {
                    root = nullptr;
                } else if (parent->left == current) {
                    parent->left = nullptr;
                } else if (parent->right == current) {
                    parent->right = nullptr;
                }
            } else if (current->left == nullptr) {
                if (parent == nullptr) {
                    root = current->right;
                } else if (parent->left == current) {
                    parent->left = current->right;
                } else if (parent->right == current) {
                    parent->right = current->right;
                }
            } else if (current->right == nullptr) {
                if (parent == nullptr) {
                    root = current