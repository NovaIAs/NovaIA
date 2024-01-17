```c#
// Import necessary libraries
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComplexCSharpCode
{
    // Define a custom exception class
    public class InvalidInputException : Exception
    {
        public InvalidInputException(string message) : base(message)
        {
        }
    }

    // Define a delegate for a function that takes two integers and returns an integer
    public delegate int BinaryOperator(int x, int y);

    // Define a class to represent a binary tree node
    public class BinaryTreeNode<T>
    {
        public T Value { get; set; }
        public BinaryTreeNode<T> Left { get; set; }
        public BinaryTreeNode<T> Right { get; set; }

        public BinaryTreeNode(T value)
        {
            Value = value;
            Left = null;
            Right = null;
        }
    }

    // Define a class to represent a binary tree
    public class BinaryTree<T>
    {
        public BinaryTreeNode<T> Root { get; set; }

        public BinaryTree()
        {
            Root = null;
        }

        // Insert a new value into the binary tree
        public void Insert(T value)
        {
            BinaryTreeNode<T> newNode = new BinaryTreeNode<T>(value);
            InsertNode(newNode);
        }

        // Helper method to insert a node into the binary tree
        private void InsertNode(BinaryTreeNode<T> node)
        {
            if (Root == null)
            {
                Root = node;
                return;
            }

            BinaryTreeNode<T> current = Root;
            while (true)
            {
                if (node.Value.CompareTo(current.Value) < 0)
                {
                    if (current.Left == null)
                    {
                        current.Left = node;
                        return;
                    }
                    else
                    {
                        current = current.Left;
                    }
                }
                else
                {
                    if (current.Right == null)
                    {
                        current.Right = node;
                        return;
                    }
                    else
                    {
                        current = current.Right;
                    }
                }
            }
        }

        // Search for a value in the binary tree
        public bool Search(T value)
        {
            return SearchNode(value, Root);
        }

        // Helper method to search for a value in the binary tree
        private bool SearchNode(T value, BinaryTreeNode<T> node)
        {
            if (node == null)
            {
                return false;
            }

            if (node.Value.Equals(value))
            {
                return true;
            }
            else if (value.CompareTo(node.Value) < 0)
            {
                return SearchNode(value, node.Left);
            }
            else
            {
                return SearchNode(value, node.Right);
            }
        }

        // Delete a value from the binary tree
        public void Delete(T value)
        {
            Root = DeleteNode(value, Root);
        }

        // Helper method to delete a node from the binary tree
        private BinaryTreeNode<T> DeleteNode(T value, BinaryTreeNode<T> node)
        {
            if (node == null)
            {
                return null;
            }

            if (value.CompareTo(node.Value) < 0)
            {
                node.Left = DeleteNode(value, node.Left);
            }
            else if (value.CompareTo(node.Value) > 0)
            {
                node.Right = DeleteNode(value, node.Right);
            }
            else
            {
                if (node.Left == null)
                {
                    return node.Right;
                }
                else if (node.Right == null)
                {
                    return node.Left;
                }

                // Find the minimum value in the right subtree
                BinaryTreeNode<T> minNode = node.Right;
                while (minNode.Left != null)
                {
                    minNode = minNode.Left;
                }

                // Copy the minimum value to the current node
                node.Value = minNode.Value;

                // Delete the minimum value from the right subtree
                node.Right = DeleteNode(minNode.Value, node.Right);
            }

            return node;
        }

        // Find the minimum value in the binary tree
        public T FindMin()
        {
            return FindMinNode(Root).Value;
        }

        // Helper method to find the minimum node in the binary tree
        private BinaryTreeNode<T> FindMinNode(BinaryTreeNode<T> node)
        {
            if (node == null)
            {
                return null;
            }

            while (node.Left != null)
            {
                node = node.Left;
            }

            return node;
        }

        // Find the maximum value in the binary tree
        public T FindMax()
        {
            return FindMaxNode(Root).Value