```d
import std.stdio, std.algorithm, std.container;

// Crea una clase de árbol binario de búsqueda.
class BSTree {
    private struct Node {
        int value;
        Node *left, *right;

        Node(int value) {
            this.value = value;
            this.left = null;
            this.right = null;
        }
    }

    private Node *root;

public:
    BSTree() {
        this.root = null;
    }

    // Inserta un valor en el árbol.
    void insert(int value) {
        if (this.root == null) {
            this.root = new Node(value);
        } else {
            insert(this.root, value);
        }
    }

    // Inserta un valor en el árbol de forma recursiva.
    private void insert(Node *node, int value) {
        if (value < node.value) {
            if (node.left == null) {
                node.left = new Node(value);
            } else {
                insert(node.left, value);
            }
        } else {
            if (node.right == null) {
                node.right = new Node(value);
            } else {
                insert(node.right, value);
            }
        }
    }

    // Busca un valor en el árbol.
    bool search(int value) {
        return search(this.root, value);
    }

    // Busca un valor en el árbol de forma recursiva.
    private bool search(Node *node, int value) {
        if (node == null) {
            return false;
        } else if (value == node.value) {
            return true;
        } else if (value < node.value) {
            return search(node.left, value);
        } else {
            return search(node.right, value);
        }
    }

    // Elimina un valor del árbol.
    void delete(int value) {
        this.root = delete(this.root, value);
    }

    // Elimina un valor del árbol de forma recursiva.
    private Node *delete(Node *node, int value) {
        if (node == null) {
            return null;
        } else if (value == node.value) {
            if (node.left == null && node.right == null) {
                return null;
            } else if (node.left == null) {
                return node.right;
            } else if (node.right == null) {
                return node.left;
            } else {
                Node *min = findMin(node.right);
                node.value = min.value;
                node.right = delete(node.right, min.value);
                return node;
            }
        } else if (value < node.value) {
            node.left = delete(node.left, value);
        } else {
            node.right = delete(node.right, value);
        }

        return node;
    }

    // Busca el valor mínimo en el árbol.
    int findMin() {
        return findMin(this.root);
    }

    // Busca el valor mínimo en el árbol de forma recursiva.
    private int findMin(Node *node) {
        if (node == null) {
            return -1;
        } else if (node.left == null) {
            return node.value;
        } else {
            return findMin(node.left);
        }
    }

    // Busca el valor máximo en el árbol.
    int findMax() {
        return findMax(this.root);
    }

    // Busca el valor máximo en el árbol de forma recursiva.
    private int findMax(Node *node) {
        if (node == null) {
            return -1;
        } else if (node.right == null) {
            return node.value;
        } else {
            return findMax(node.right);
        }
    }

    // Imprime el