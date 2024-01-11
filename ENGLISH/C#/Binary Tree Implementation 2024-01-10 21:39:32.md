```csharp
// Import necessary libraries
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Define a delegate for a function that takes two integers and returns an integer
delegate int BinaryOperator(int x, int y);

// Define a class to represent a binary tree node
class Node
{
    public int Value { get; set; }
    public Node Left { get; set; }
    public Node Right { get; set; }

    public Node(int value)
    {
        Value = value;
        Left = null;
        Right = null;
    }
}

// Define a class to represent a binary tree
class BinaryTree
{
    public Node Root { get; set; }

    public BinaryTree()
    {
        Root = null;
    }

    // Insert a value into the binary tree
    public void Insert(int value)
    {
        Node newNode = new Node(value);
        if (Root == null)
        {
            Root = newNode;
        }
        else
        {
            InsertRecursive(newNode, Root);
        }
    }

    // Insert a node into the binary tree recursively
    private void InsertRecursive(Node newNode, Node currentNode)
    {
        if (newNode.Value < currentNode.Value)
        {
            if (currentNode.Left == null)
            {
                currentNode.Left = newNode;
            }
            else
            {
                InsertRecursive(newNode, currentNode.Left);
            }
        }
        else
        {
            if (currentNode.Right == null)
            {
                currentNode.Right = newNode;
            }
            else
            {
                InsertRecursive(newNode, currentNode.Right);
            }
        }
    }

    // Search for a value in the binary tree
    public bool Search(int value)
    {
        return SearchRecursive(value, Root);
    }

    // Search for a value in the binary tree recursively
    private bool SearchRecursive(int value, Node currentNode)
    {
        if (currentNode == null)
        {
            return false;
        }
        else if (value == currentNode.Value)
        {
            return true;
        }
        else if (value < currentNode.Value)
        {
            return SearchRecursive(value, currentNode.Left);
        }
        else
        {
            return SearchRecursive(value, currentNode.Right);
        }
    }

    // Delete a value from the binary tree
    public bool Delete(int value)
    {
        return DeleteRecursive(value, Root, null);
    }

    // Delete a value from the binary tree recursively
    private bool DeleteRecursive(int value, Node currentNode, Node parentNode)
    {
        if (currentNode == null)
        {
            return false;
        }
        else if (value == currentNode.Value)
        {
            if (currentNode.Left == null && currentNode.Right == null)
            {
                if (parentNode == null)
                {
                    Root = null;
                }
                else if (parentNode.Left == currentNode)
                {
                    parentNode.Left = null;
                }
                else
                {
                    parentNode.Right = null;
                }
            }
            else if (currentNode.Left == null)
            {
                if (parentNode == null)
                {
                    Root = currentNode.Right;
                }
                else if (parentNode.Left == currentNode)
                {
                    parentNode.Left = currentNode.Right;
                }
                else
                {
                    parentNode.Right = currentNode.Right;
                }
            }
            else if (currentNode.Right == null)
            {
                if (parentNode == null)
                {
                    Root = currentNode.Left;
                }
                else if (parentNode.Left == currentNode)
                {
                    parentNode.Left = currentNode.Left;
                }
                else
                {
                    parentNode.Right = currentNode.Left;
                }
            }
            else
            {
                Node successor = FindSuccessor(currentNode);
                currentNode.Value = successor.Value;
                DeleteRecursive(successor.Value, currentNode.Right, currentNode);
            }
            return true;
        }
        else if (value < currentNode.Value)
        {
            return DeleteRecursive(value, currentNode.Left, currentNode);
        }
        else
        {
            return DeleteRecursive(value, currentNode.Right, currentNode);
        }
    }

    // Find the successor of a node in the binary tree
    private Node FindSuccessor(Node node)
    {
        Node current