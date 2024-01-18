```c#
// This is a complex C# code that implements a binary tree data structure.
// A binary tree is a hierarchical data structure used to organize and store data.
// Each node in the tree can have a maximum of two child nodes, one on the left and one on the right.
// The code includes methods for adding, searching, and deleting nodes from the tree,
// as well as methods for traversing the tree in different orders (pre-order, in-order, post-order).

using System;
using System.Collections.Generic;

public class BinaryTree<T>
{
    private Node<T> root; // The root node of the binary tree.

    public BinaryTree()
    {
        root = null; // Initialize the root node to null.
    }

    public void Add(T value)
    {
        Node<T> newNode = new Node<T>(value); // Create a new node with the given value.

        if (root == null) // If the tree is empty, set the new node as the root node.
        {
            root = newNode;
        }
        else
        {
            Node<T> currentNode = root; // Start from the root node.

            while (true)
            {
                if (value.CompareTo(currentNode.Value) < 0) // If the new value is less than the current node's value, go left.
                {
                    if (currentNode.LeftChild == null) // If the left child of the current node is null, insert the new node as the left child.
                    {
                        currentNode.LeftChild = newNode;
                        break;
                    }
                    else // Otherwise, move to the left child and continue searching.
                    {
                        currentNode = currentNode.LeftChild;
                    }
                }
                else // If the new value is greater than or equal to the current node's value, go right.
                {
                    if (currentNode.RightChild == null) // If the right child of the current node is null, insert the new node as the right child.
                    {
                        currentNode.RightChild = newNode;
                        break;
                    }
                    else // Otherwise, move to the right child and continue searching.
                    {
                        currentNode = currentNode.RightChild;
                    }
                }
            }
        }
    }

    public bool Search(T value)
    {
        Node<T> currentNode = root; // Start from the root node.

        while (currentNode != null)
        {
            if (value.CompareTo(currentNode.Value) == 0) // If the value is equal to the current node's value, return true.
            {
                return true;
            }
            else if (value.CompareTo(currentNode.Value) < 0) // If the value is less than the current node's value, go left.
            {
                currentNode = currentNode.LeftChild;
            }
            else // If the value is greater than the current node's value, go right.
            {
                currentNode = currentNode.RightChild;
            }
        }

        return false; // If the value is not found, return false.
    }

    public void Delete(T value)
    {
        Node<T> currentNode = root; // Start from the root node.
        Node<T> parentNode = null; // The parent node of the current node.

        while (currentNode != null)
        {
            if (value.CompareTo(currentNode.Value) == 0) // If the value is equal to the current node's value, break out of the loop.
            {
                break;
            }
            else if (value.CompareTo(currentNode.Value) < 0) // If the value is less than the current node's value, go left.
            {
                parentNode = currentNode;
                currentNode = currentNode.LeftChild;
            }
            else // If the value is greater than the current node's value, go right.
            {
                parentNode = currentNode;
                currentNode = currentNode.RightChild;
            }
        }

        if (currentNode == null) // If the value is not found, return without doing anything.
        {
            return;
        }

        if (currentNode.LeftChild == null && currentNode.RightChild == null) // If the current node has no children, simply remove it from the tree.
        {
            if (parentNode == null) // If the current node is the root node, set the root node to null.
            {
                root = null;
            }
            else if (parentNode.LeftChild == currentNode) // If the current node is the left child of its parent, set the parent's left child to null.
            {
                parentNode.LeftChild = null;
            }
            else // If the current node is the right child of its parent, set the parent's right child to null.
            {
                parentNode.RightChild = null;
            }
        }
        else if (currentNode.LeftChild != null && currentNode.RightChild == null) // If the current node has only a left child, replace the current node with its left child.
        {
            if (parentNode == null) // If the current node is the root node, set the root node to its left child.
            {
                root = currentNode.LeftChild;
            }
            else if (parentNode.LeftChild == currentNode) // If the current node is the left child of its parent, set the parent's left child to its left child.
            {
                parentNode.LeftChild = currentNode.LeftChild;
            }
            else // If the current node is the right child of its parent, set the parent's right child to its left child.
            {
                parentNode.RightChild = currentNode.LeftChild;
            }
        }
        else if (currentNode.LeftChild == null && currentNode.RightChild != null) // If the current node has only a right child, replace the current node with its right child.
        {
            if (parentNode == null) // If the current node is the root node, set the root node to its right child.
            {
                root = currentNode.RightChild;
            }
            else if (parentNode.LeftChild == currentNode) // If the current node is the left child of its parent, set the parent's left child to its right child.
            {
                parentNode.LeftChild = currentNode.RightChild;
            }
            else // If the current node is the right child of its parent, set the parent's right child to its right child.
            {
                parentNode.RightChild = currentNode.RightChild;
            }
        }
        else // If the current node has both a left and a right child, replace the current node with its successor (the leftmost node in the right subtree)