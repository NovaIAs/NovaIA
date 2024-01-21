```d
import std.stdio, std.algorithm, std.string;

// Construye un árbol binario de búsqueda.
class BinarySearchTree {
protected:
    struct Node {
        int key;
        Node* left, *right;

        Node(int key) {
            this.key = key;
            this.left = null;
            this.right = null;
        }
    }

    Node* root;

public:
    bool isEmpty() {
        return root == null;
    }

    void insert(int key) {
        insert(key, root);
    }

protected:
    void insert(int key, Node*& node) {
        if (node == null) {
            node = new Node(key);
        } else if (key < node.key) {
            insert(key, node.left);
        } else if (key > node.key) {
            insert(key, node.right);
        }
    }
}

// Implementa un algoritmo de ordenación por selección.
class SelectionSort {
public:
    static void sort(in, out T[])(T[] in, out T[] out) {
        for (int i = 0; i < in.length; i++) {
            int minIndex = i;
            for (int j = i + 1; j < in.length; j++) {
                if (in[j] < in[minIndex]) {
                    minIndex = j;
                }
            }
            T temp = in[i];
            in[i] = in[minIndex];
            in[minIndex] = temp;
        }
    }
}

// Implementa un algoritmo de búsqueda binaria.
class BinarySearch {
public:
    static int search(in T[])(T[] in, T key) pure nothrow @safe {
        int low = 0;
        int high = in.length - 1;
        while (low <= high) {
            int mid = (low + high) / 2;
            if (in[mid] == key) {
                return mid;
            } else if (in[mid] < key) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        return -1;
    }
}

// Implementa una estructura de datos de lista enlazada.
class LinkedList {
protected:
    struct Node {
        int data;
        Node* next;

        Node(int data) {
            this.data = data;
            this.next = null;
        }
    }

    Node* head;

public:
    bool isEmpty() {
        return head == null;
    }

    void insert(int data) {
        Node* new_node = new Node(data);
        new_node.next = head;
        head = new_node;
    }

    int delete(int data) {
        if (head == null) {
            return -1;
        }
        Node* prev = null;
        Node* curr = head;
        while (curr != null) {
            if (curr.data == data) {
                if (prev == null) {
                    head = curr.next;
                } else {
                    prev.next = curr.next;
                }
                return data;
            }
            prev = curr;
            curr = curr.next;
        }
        return -1;
    }

    void print() {
        Node* curr = head;
        while (curr != null) {
            write(curr.data, " ");
            curr = curr.next;
        }
        writeln();
    }
}

void main() {
    BinarySearchTree bst;
    bst.insert(10);
    bst.insert(5);
    bst.insert(20);
    bst.insert(3);
    bst.insert(8);
    bst.insert(15);
    bst.insert(2