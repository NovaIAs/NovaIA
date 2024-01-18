```c#
// This code implements a binary search tree in C#.
// It includes methods for inserting, searching, and deleting nodes, as well as methods for finding the minimum and maximum values in the tree.
// The tree is implemented using a recursive data structure, where each node has a left and right child.
// The left child contains nodes with values less than the parent node, and the right child contains nodes with values greater than the parent node.

using System;

public class BinarySearchTree
{
    private Node root;

    public void Insert(int value)
    {
        // If the tree is empty, create a new root node.
        if (root == null)
        {
            root = new Node(value);
            return;
        }

        // Otherwise, find the correct place to insert the new node.
        Node current = root;
        while (true)
        {
            // If the value is less than the current node's value, go to the left child.
            if (value < current.Value)
            {
                // If the left child is null, insert the new node here.
                if (current.Left == null)
                {
                    current.Left = new Node(value);
                    return;
                }

                // Otherwise, move to the left child and continue searching.
                current = current.Left;
            }

            // If the value is greater than the current node's value, go to the right child.
            else
            {
                // If the right child is null, insert the new node here.
                if (current.Right == null)
                {
                    current.Right = new Node(value);
                    return;
                }

                // Otherwise, move to the right child and continue searching.
                current = current.Right;
            }
        }
    }

    public bool Search(int value)
    {
        // Start at the root node.
        Node current = root;

        // While the current node is not null and the value has not been found, continue searching.
        while (current != null && current.Value != value)
        {
            // If the value is less than the current node's value, go to the left child.
            if (value < current.Value)
            {
                current = current.Left;
            }

            // Otherwise, go to the right child.
            else
            {
                current = current.Right;
            }
        }

        // If the current node is null, the value was not found.
        if (current == null)
        {
            return false;
        }

        // Otherwise, the value was found.
        return true;
    }

    public void Delete(int value)
    {
        // Find the node to be deleted.
        Node current = root;
        Node parent = null;
        while (current != null && current.Value != value)
        {
            // If the value is less than the current node's value, go to the left child.
            if (value < current.Value)
            {
                parent = current;
                current = current.Left;
            }

            // Otherwise, go to the right child.
            else
            {
                parent = current;
                current = current.Right;
            }
        }

        // If the node to be deleted is not found, return.
        if (current == null)
        {
            return;
        }

        // If the node to be deleted has no children, simply remove it from the tree.
        if (current.Left == null && current.Right == null)
        {
            if (parent == null)
            {
                root = null;
            }
            else
            {
                if (parent.Left == current)
                {
                    parent.Left = null;
                }
                else
                {
                    parent.Right = null;
                }
            }
        }

        // If the node to be deleted has only one child, replace it with its child.
        else if (current.Left == null)
        {
            if (parent == null)
            {
                root = current.Right;
            }
            else
            {
                if (parent.Left == current)
                {
                    parent.Left = current.Right;
                }
                else
                {
                    parent.Right = current.Right;
                }
            }
        }
        else if (current.Right == null)
        {
            if (parent == null)
            {
                root = current.Left;
            }
            else
            {
                if (parent.Left == current)
                {
                    parent.