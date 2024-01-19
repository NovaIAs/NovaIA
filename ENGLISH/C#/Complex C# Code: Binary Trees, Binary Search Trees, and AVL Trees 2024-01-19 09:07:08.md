```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComplexCSharpCode
{
    // Define a delegate for a function that takes two integers and returns an integer.
    public delegate int BinaryOperator(int x, int y);

    // Define a class to represent a binary tree node.
    public class Node
    {
        public int Value { get; set; }
        public Node Left { get; set; }
        public Node Right { get; set; }

        public Node(int value)
        {
            Value = value;
        }
    }

    // Define a class to represent a binary tree.
    public class BinaryTree
    {
        public Node Root { get; set; }

        public void Insert(int value)
        {
            if (Root == null)
            {
                Root = new Node(value);
            }
            else
            {
                Insert(Root, value);
            }
        }

        private void Insert(Node node, int value)
        {
            if (value < node.Value)
            {
                if (node.Left == null)
                {
                    node.Left = new Node(value);
                }
                else
                {
                    Insert(node.Left, value);
                }
            }
            else
            {
                if (node.Right == null)
                {
                    node.Right = new Node(value);
                }
                else
                {
                    Insert(node.Right, value);
                }
            }
        }

        public int Height()
        {
            return Height(Root);
        }

        private int Height(Node node)
        {
            if (node == null)
            {
                return 0;
            }
            else
            {
                return 1 + Math.Max(Height(node.Left), Height(node.Right));
            }
        }

        public bool IsBalanced()
        {
            return IsBalanced(Root);
        }

        private bool IsBalanced(Node node)
        {
            if (node == null)
            {
                return true;
            }
            else
            {
                int leftHeight = Height(node.Left);
                int rightHeight = Height(node.Right);
                return Math.Abs(leftHeight - rightHeight) <= 1 &&
                    IsBalanced(node.Left) &&
                    IsBalanced(node.Right);
            }
        }

        public void TraverseInOrder(BinaryOperator op)
        {
            TraverseInOrder(Root, op);
        }

        private void TraverseInOrder(Node node, BinaryOperator op)
        {
            if (node != null)
            {
                TraverseInOrder(node.Left, op);
                op(node.Value, node.Value);
                TraverseInOrder(node.Right, op);
            }
        }
    }

    // Define a class to represent a binary search tree.
    public class BinarySearchTree : BinaryTree
    {
        public bool Insert(int value)
        {
            if (Root == null)
            {
                Root = new Node(value);
                return true;
            }
            else
            {
                return Insert(Root, value);
            }
        }

        private bool Insert(Node node, int value)
        {
            if (value < node.Value)
            {
                if (node.Left == null)
                {
                    node.Left = new Node(value);
                    return true;
                }
                else
                {
                    return Insert(node.Left, value);
                }
            }
            else if (value > node.Value)
            {
                if (node.Right == null)
                {
                    node.Right = new Node(value);
                    return true;
                }
                else
                {
                    return Insert(node.Right, value);
                }
            }
            else
            {
                return false; // Duplicate values are not allowed in a binary search tree.
            }
        }

        public bool Search(int value)
        {
            return Search(Root, value);
        }

        private bool Search(Node node, int value)
        {
            if (node == null)
            {
                return false;
            }
            else if (value < node.Value)
            {
                return Search(node.Left, value);
            }
            else if (value > node.Value)
            {
                return Search(node.Right, value);
            }
            else
            {
                return true;
            }
        }
    }

    // Define a class to represent an AVL tree.
    public class AvlTree : BinarySearchTree
    {
        public new bool Insert(int value)
        {
            if (Root == null)
            {
                Root = new Node(value);
                return true;
            }
            else
            {
                return Insert(Root, value);
            }
        }

        private new bool Insert(Node node, int value)
        {
            if (value < node.Value)
            {
                if (node.Left