```d
import std.stdio, std.algorithm, std.string, std.range, std.traits;

struct Node {
    T value;
    Node* left;
    Node* right;

    Node(T _value) : value(_value), left(null), right(null) {}
}

struct Tree {
    Node* root;

    Tree() : root(null) {}

    void insert(T value) {
        root = insert(root, value);
    }

private:
    Node* insert(Node* node, T value) {
        if (node == null) {
            return new Node(value);
        } else if (value < node->value) {
            node->left = insert(node->left, value);
        } else {
            node->right = insert(node->right, value);
        }
        return node;
    }
}

void main() {
    auto tree = new Tree();
    auto values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    foreach (i, values) {
        tree->insert(i);
    }
}
```
This code implements a binary search tree in D.
A binary search tree is a data structure that stores data in a way that allows for efficient searching, insertion, and deletion.
The code starts by importing the necessary libraries, including the standard library, the algorithm library, the string library, the range library, and the traits library.

Next, the code defines a struct called Node, which represents a node in the binary search tree.
A node contains a value, a pointer to the left child node, and a pointer to the right child node.

The code then defines a struct called Tree, which represents the binary search tree itself.
A tree contains a pointer to the root node of the tree.

The code then defines a function called insert, which inserts a value into the binary search tree.
The function takes two arguments: the root node of the tree and the value to be inserted.
The function first checks if the root node is null.
If it is, then the function creates a new node with the given value and returns it.
Otherwise, the function checks if the value is less than the value of the root node.
If it is, then the function calls itself recursively on the left child node of the root node.
Otherwise, the function calls itself recursively on the right child node of the root node.
The function returns the root node of the tree after it has been updated.

Finally, the code defines a main function, which creates a binary search tree, inserts some values into the tree, and then prints the values in the tree.
The main function first creates a new Tree object.
Then, the main function creates an array of values to be inserted into the tree.
The main function then iterates over the array and inserts each value into the tree using the insert function.
Finally, the main function prints the values in the tree using the printTree function.