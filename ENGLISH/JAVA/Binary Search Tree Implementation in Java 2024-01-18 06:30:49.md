```java
import java.util.*;
import java.lang.*;

class Node {
    int data;
    Node left;
    Node right;

    public Node(int data) {
        this.data = data;
        this.left = null;
        this.right = null;
    }
}

class BinaryTree {
    Node root;

    public BinaryTree() {
        this.root = null;
    }

    public void insert(int data) {
        Node newNode = new Node(data);
        if (this.root == null) {
            this.root = newNode;
        } else {
            insertHelper(this.root, newNode);
        }
    }

    private void insertHelper(Node currentNode, Node newNode) {
        if (newNode.data < currentNode.data) {
            if (currentNode.left == null) {
                currentNode.left = newNode;
            } else {
                insertHelper(currentNode.left, newNode);
            }
        } else {
            if (currentNode.right == null) {
                currentNode.right = newNode;
            } else {
                insertHelper(currentNode.right, newNode);
            }
        }
    }

    public boolean search(int data) {
        return searchHelper(this.root, data);
    }

    private boolean searchHelper(Node currentNode, int data) {
        if (currentNode == null) {
            return false;
        }
        if (currentNode.data == data) {
            return true;
        }
        if (data < currentNode.data) {
            return searchHelper(currentNode.left, data);
        } else {
            return searchHelper(currentNode.right, data);
        }
    }

    public void delete(int data) {
        deleteHelper(this.root, data);
    }

    private Node deleteHelper(Node currentNode, int data) {
        if (currentNode == null) {
            return null;
        }
        if (data < currentNode.data) {
            currentNode.left = deleteHelper(currentNode.left, data);
        } else if (data > currentNode.data) {
            currentNode.right = deleteHelper(currentNode.right, data);
        } else {
            if (currentNode.left == null) {
                return currentNode.right;
            } else if (currentNode.right == null) {
                return currentNode.left;
            }
            currentNode.data = findMin(currentNode.right).data;
            currentNode.right = deleteHelper(currentNode.right, currentNode.data);
        }
        return currentNode;
    }

    private Node findMin(Node currentNode) {
        if (currentNode.left == null) {
            return currentNode;
        } else {
            return findMin(currentNode.left);
        }
    }

    public void printPreorder() {
        printPreorderHelper(this.root);
        System.out.println();
    }

    private void printPreorderHelper(Node currentNode) {
        if (currentNode != null) {
            System.out.print(currentNode.data + " ");
            printPreorderHelper(currentNode.left);
            printPreorderHelper(currentNode.right);
        }
    }

    public void printInorder() {
        printInorderHelper(this.root);
        System.out.println();
    }

    private void printInorderHelper(Node currentNode) {
        if (currentNode != null) {
            printInorderHelper(currentNode.left);
            System.out.print(currentNode.data + " ");
            printInorderHelper(currentNode.right);
        }
    }

    public void printPostorder() {
        printPostorderHelper(this.root);
        System.out.println();
    }

    private void printPostorderHelper(Node currentNode) {
        if (currentNode != null) {
            printPostorderHelper(currentNode.left);
            printPostorderHelper(currentNode.right);
            System.out.print(currentNode.data + " ");
        }
    }
}

public class Main {
    public static void main(String[] args) {
        BinaryTree tree = new BinaryTree();
        tree.insert(10);
        tree.insert(5);
        tree.insert(15);
        tree.insert(2);
        tree.insert(7);
        tree.insert(12);
        tree.insert(20);

        System.out.print("Preorder: ");
        tree.printPreorder();

        System.out.print("Inorder: ");
        tree.printInorder();

        System.out.print("Postorder: ");
        tree.printPostorder();

        System.out.println("Searching for 15: " + tree.search(15));
        System.out.println("Searching for 25: " + tree.search(25));

        tree.delete(15);

        System.out.print("Preorder after deleting 15: ");
        tree.printPreorder();

        System.out.print("Inorder after deleting 15: ");
        tree.printInorder();

        System.out.print("Postorder after deleting 15: ");
        tree.printPostorder();
    }
}
```

This code implements a binary search tree in Java. A binary search tree is a data structure that stores data in a way that allows for efficient searching and retrieval. The data is stored in nodes, which are connected to each other by edges. Each node has a value, and the value of a node is greater than the values of its left child and less than the values of its right child.

The code has a class called `Node`, which represents a single node in the tree. The `Node` class has three fields: `data`, `left`, and `right`. The `data` field stores the value of the node, the `left` field stores the left child of the node, and the `right` field stores the right child of the node.

The code also has a class called `BinaryTree`, which represents the entire binary search tree. The `BinaryTree` class has a field called `root`, which stores the root node of the tree. The `root` node is the topmost node in the tree, and all other nodes in the tree are descendants of the root node.

The `BinaryTree` class has a number of methods that can be used to manipulate the tree. The `insert` method inserts a new node into the tree. The `search` method searches for a node in the tree. The `delete` method deletes a node from the tree. The `printPreorder`, `printInorder`, and `printPostorder` methods print the nodes of the tree in preorder, inorder, and postorder, respectively.

The `main` method of the program creates a new binary search tree and inserts a number of values into it. The `main` method then calls the `printPreorder`, `printInorder`, and `printPostorder` methods to print the nodes of the tree in preorder, inorder, and postorder, respectively. The `main` method also searches for a value in the tree and deletes a value from the tree.