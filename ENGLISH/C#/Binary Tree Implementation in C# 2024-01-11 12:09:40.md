```csharp
// Import necessary libraries
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Define a delegate for a function that takes two integers and returns an integer
delegate int BinaryOperation(int a, int b);

// Define a class to represent a binary tree node
class Node
{
    public int Value { get; set; }
    public Node Left { get; set; }
    public Node Right { get; set; }

    public Node(int value)
    {
        Value = value;
    }
}

// Define a class to represent a binary tree
class BinaryTree
{
    public Node Root { get; set; }

    // Insert a new value into the binary tree
    public void Insert(int value)
    {
        if (Root == null)
        {
            Root = new Node(value);
            return;
        }

        Insert(value, Root);
    }

    // Recursive helper method to insert a new value into the binary tree
    private void Insert(int value, Node node)
    {
        if (value < node.Value)
        {
            if (node.Left == null)
            {
                node.Left = new Node(value);
                return;
            }
            else
            {
                Insert(value, node.Left);
            }
        }
        else
        {
            if (node.Right == null)
            {
                node.Right = new Node(value);
                return;
            }
            else
            {
                Insert(value, node.Right);
            }
        }
    }

    // Search for a value in the binary tree
    public bool Search(int value)
    {
        if (Root == null)
        {
            return false;
        }

        return Search(value, Root);
    }

    // Recursive helper method to search for a value in the binary tree
    private bool Search(int value, Node node)
    {
        if (node == null)
        {
            return false;
        }

        if (value == node.Value)
        {
            return true;
        }
        else if (value < node.Value)
        {
            return Search(value, node.Left);
        }
        else
        {
            return Search(value, node.Right);
        }
    }

    // Traverse the binary tree in inorder (left, root, right)
    public void InorderTraversal()
    {
        if (Root == null)
        {
            return;
        }

        InorderTraversal(Root);
    }

    // Recursive helper method to traverse the binary tree in inorder (left, root, right)
    private void InorderTraversal(Node node)
    {
        if (node == null)
        {
            return;
        }

        InorderTraversal(node.Left);
        Console.WriteLine(node.Value);
        InorderTraversal(node.Right);
    }

    // Traverse the binary tree in preorder (root, left, right)
    public void PreorderTraversal()
    {
        if (Root == null)
        {
            return;
        }

        PreorderTraversal(Root);
    }

    // Recursive helper method to traverse the binary tree in preorder (root, left, right)
    private void PreorderTraversal(Node node)
    {
        if (node == null)
        {
            return;
        }

        Console.WriteLine(node.Value);
        PreorderTraversal(node.Left);
        PreorderTraversal(node.Right);
    }

    // Traverse the binary tree in postorder (left, right, root)
    public void PostorderTraversal()
    {
        if (Root == null)
        {
            return;
        }

        PostorderTraversal(Root);
    }

    // Recursive helper method to traverse the binary tree in postorder (left, right, root)
    private void PostorderTraversal(Node node)
    {
        if (node == null)
        {
            return;
        }

        PostorderTraversal(node.Left);
        PostorderTraversal(node.Right);
        Console.WriteLine(node.Value);
    }

    // Find the minimum value in the binary tree
    public int FindMin()
    {
        if (Root == null)
        {
            throw new InvalidOperationException("Binary tree is empty.");
        }

        return FindMin(Root);
    }

    // Recursive helper method to find the minimum value in the binary tree