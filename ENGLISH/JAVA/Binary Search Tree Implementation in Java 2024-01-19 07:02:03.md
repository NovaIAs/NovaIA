```java
import java.util.*;
import java.lang.*;
import java.math.*;

public class VeryLargeAndDifferentiatedCode {

    // A class to represent a node in a binary tree
    private static class Node {
        int data;
        Node left;
        Node right;

        public Node(int data) {
            this.data = data;
            this.left = null;
            this.right = null;
        }
    }

    // A class to represent a binary search tree
    private static class BinarySearchTree {

        private Node root;

        public BinarySearchTree() {
            this.root = null;
        }

        public void insert(int data) {
            root = insert(root, data);
        }

        private Node insert(Node node, int data) {
            if (node == null) {
                return new Node(data);
            } else if (data <= node.data) {
                node.left = insert(node.left, data);
            } else {
                node.right = insert(node.right, data);
            }
            return node;
        }

        public boolean search(int data) {
            return search(root, data);
        }

        private boolean search(Node node, int data) {
            if (node == null) {
                return false;
            } else if (data == node.data) {
                return true;
            } else if (data <= node.data) {
                return search(node.left, data);
            } else {
                return search(node.right, data);
            }
        }

        public void delete(int data) {
            root = delete(root, data);
        }

        private Node delete(Node node, int data) {
            if (node == null) {
                return null;
            } else if (data < node.data) {
                node.left = delete(node.left, data);
            } else if (data > node.data) {
                node.right = delete(node.right, data);
            } else {
                if (node.left == null) {
                    return node.right;
                } else if (node.right == null) {
                    return node.left;
                } else {
                    node.data = findMin(node.right).data;
                    node.right = delete(node.right, node.data);
                }
            }
            return node;
        }

        private Node findMin(Node node) {
            if (node.left == null) {
                return node;
            } else {
                return findMin(node.left);
            }
        }

        public void printInOrder() {
            printInOrder(root);
        }

        private void printInOrder(Node node) {
            if (node == null) {
                return;
            }
            printInOrder(node.left);
            System.out.print(node.data + " ");
            printInOrder(node.right);
        }

        public void printPreOrder() {
            printPreOrder(root);
        }

        private void printPreOrder(Node node) {
            if (node == null) {
                return;
            }
            System.out.print(node.data + " ");
            printPreOrder(node.left);
            printPreOrder(node.right);
        }

        public void printPostOrder() {
            printPostOrder(root);
        }

        private void printPostOrder(Node node) {
            if (node == null) {
                return;
            }
            printPostOrder(node.left);
            printPostOrder(node.right);
            System.out.print(node.data + " ");
        }

        public int size() {
            return size(root);
        }

        private int size(Node node) {
            if (node == null) {
                return 0;
            } else {
                return 1 + size(node.left) + size(node.right);
            }
        }

        public int height() {
            return height