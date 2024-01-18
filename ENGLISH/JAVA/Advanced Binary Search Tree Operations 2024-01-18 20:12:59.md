```java
import java.util.*;
import java.io.*;

public class AdvancedBinarySearchTree {

    // Node of the Binary Search Tree
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

    // Root of the Binary Search Tree
    private Node root;

    // Insert a new node with the given value into the Binary Search Tree
    public void insert(int value) {
        root = insert(root, value);
    }

    // Recursive helper method to insert a new node with the given value into the Binary Search Tree
    private Node insert(Node node, int value) {
        if (node == null) {
            return new Node(value);
        } else if (value < node.data) {
            node.left = insert(node.left, value);
        } else if (value > node.data) {
            node.right = insert(node.right, value);
        } else {
            // Duplicate values are not allowed in the Binary Search Tree
            throw new IllegalArgumentException("Duplicate values are not allowed in the Binary Search Tree");
        }
        return node;
    }

    // Find a node with the given value in the Binary Search Tree
    public Node find(int value) {
        return find(root, value);
    }

    // Recursive helper method to find a node with the given value in the Binary Search Tree
    private Node find(Node node, int value) {
        if (node == null) {
            return null;
        } else if (value < node.data) {
            return find(node.left, value);
        } else if (value > node.data) {
            return find(node.right, value);
        } else {
            return node;
        }
    }

    // Delete a node with the given value from the Binary Search Tree
    public void delete(int value) {
        root = delete(root, value);
    }

    // Recursive helper method to delete a node with the given value from the Binary Search Tree
    private Node delete(Node node, int value) {
        if (node == null) {
            return null;
        } else if (value < node.data) {
            node.left = delete(node.left, value);
        } else if (value > node.data) {
            node.right = delete(node.right, value);
        } else {
            // Node to be deleted has two children
            if (node.left != null && node.right != null) {
                // Find the minimum value in the right subtree
                int minValue = findMinValue(node.right);
                // Replace the value of the node to be deleted with the minimum value
                node.data = minValue;
                // Delete the node with the minimum value from the right subtree
                node.right = delete(node.right, minValue);
            } else {
                // Node to be deleted has one or no children
                if (node.left != null) {
                    return node.left;
                } else {
                    return node.right;
                }
            }
        }
        return node;
    }

    // Find the minimum value in the Binary Search Tree
    private int findMinValue(Node node) {
        if (node == null) {
            return Integer.MAX_VALUE;
        } else if (node.left == null) {
            return node.data;
        } else {
            return findMinValue(node.left);
        }
    }

    // Print the Binary Search Tree in pre-order traversal
    public void printPreorder() {
        printPreorder(root);
    }

    // Recursive helper method to print the Binary Search Tree in pre-order traversal
    private void printPreorder(Node node) {
        if (node != null) {
            System.out.print(node.data + " ");
            printPreorder(node.left);
            printPreorder(node.right);
        }
    }

    // Print the Binary Search Tree in in-order traversal
    public void printInorder() {
        printInorder(root);
    }

    // Recursive helper method to print the Binary Search Tree in in-order traversal
    private void printInorder(Node node) {
        if (node != null) {
            printInorder(node.left);
            System.out.print(node.data + " ");
            printInorder(node.right);
        }
    }

    // Print the Binary Search Tree in post-order traversal
    public void printPostorder() {
        printPostorder(root);
    }

    // Recursive helper method to print the Binary Search Tree in post-order traversal
    private void printPostorder(Node node) {
        if (node != null) {
            printPostorder(node.left);
            printPostorder(node.right);
            System.out.print(node.data + " ");
        }
    }

    // Check if the Binary Search Tree is balanced
    public boolean isBalanced() {
        return isBalanced(root) != -1;
    }

    // Recursive helper method to check if the Binary Search Tree is balanced
    private int isBalanced(Node node) {
        if (node == null) {
            return 0;
        } else {
            int leftHeight = isBalanced(node.left);
            int rightHeight = isBalanced(node.right);
            if (leftHeight == -1 || rightHeight == -1) {
                return -1;
            } else {
                int diff = Math.abs(leftHeight - rightHeight);
                if (diff > 1) {
                    return -1;
                } else {
                    return Math.max(leftHeight, rightHeight) + 1;
                }
            }
        }
    }

    // Check if the Binary Search Tree is a valid Binary Search Tree
    public boolean isValidBST() {
        return isValidBST(root, Integer.MIN_VALUE, Integer.MAX_VALUE);
    }

    // Recursive helper method to check if the Binary Search Tree is a valid Binary Search Tree
    private boolean isValidBST(Node node, int minValue, int maxValue) {
        if (node == null) {
            return true;
        } else {
            if (node.data < minValue || node.data > maxValue) {
                return false;
            } else {
                return isValidBST(node.left, minValue, node.data) && isValidBST(node.right, node.data, maxValue);
            }
        }
    }

    // Find the range of values in the Binary Search Tree
    public Range findRange() {
        return findRange(root);
    }

    // Recursive helper method to find the range of values in the Binary Search Tree
    private Range findRange(Node node) {
        if (node == null) {
            return new Range(Integer.MAX_VALUE, Integer.MIN_VALUE);
        } else {
            Range leftRange = findRange(node.left);
            Range rightRange = findRange(node.right);
            Range range = new Range(Math.min(leftRange.min, rightRange.min), Math.max(leftRange.max, rightRange.max));
            return range;
        }
    }

    // Print the level order traversal of the Binary Search Tree
    public void levelOrderTraversal() {
        Queue<Node> queue = new LinkedList<>();
        queue.add(root);
        while (!queue.isEmpty()) {
            Node node = queue.poll();
            System.out.print(node.data + " ");
            if (node.left != null) {
                queue.add(node.left);
            }
            if (node.right != null) {
                queue.add(node.right);
            }
        }
    }

    // Find the height of the Binary Search Tree
    public int height() {
        return height(root);
    }

    // Recursive helper method to find the height of the Binary Search Tree
    private int height(Node node) {
        if (node == null) {
            return 0;
        } else {
            int leftHeight = height(node.left);
            int rightHeight = height(node.right);
            return Math.max(leftHeight, rightHeight) + 1;
        }
    }

    // Find the diameter of the Binary Search Tree
    public int diameter() {
        return diameter(root);
    }

    // Recursive helper method to find the diameter of the Binary Search Tree
    private int diameter(Node node) {
        if (node == null) {
            return 0;
        } else {
            int leftDiameter = diameter(node.left);
            int rightDiameter = diameter(node.right);
            int leftHeight = height(node.left);
            int rightHeight = height(node.right);
            int diameterThroughNode = leftHeight + rightHeight + 1;
            return Math.max(diameterThroughNode, Math.max(leftDiameter, rightDiameter));
        }
    }

    // Find the lowest common ancestor of two nodes in the Binary Search Tree
    public Node lowestCommonAncestor(int value1, int value2) {
        return lowestCommonAncestor(root, value1, value2);
    }

    // Recursive helper method to find the lowest common ancestor of two nodes in the Binary