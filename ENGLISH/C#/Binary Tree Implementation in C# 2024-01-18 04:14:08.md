```csharp
// This is a class to represent a Binary Tree
public class BinaryTree
{
    // The data of the node
    public int data;

    // The left child of the node
    public BinaryTree left;

    // The right child of the node
    public BinaryTree right;

    // Constructor to create a new node
    public BinaryTree(int data)
    {
        this.data = data;
        this.left = null;
        this.right = null;
    }

    // Method to insert a new node into the tree
    public void Insert(int data)
    {
        // If the tree is empty, create a new node as the root
        if (this.data == null)
        {
            this.data = data;
        }
        // Otherwise, insert the node into the left or right subtree
        else if (data < this.data)
        {
            if (this.left == null)
            {
                this.left = new BinaryTree(data);
            }
            else
            {
                this.left.Insert(data);
            }
        }
        else
        {
            if (this.right == null)
            {
                this.right = new BinaryTree(data);
            }
            else
            {
                this.right.Insert(data);
            }
        }
    }

    // Method to search for a node in the tree
    public bool Search(int data)
    {
        // If the tree is empty, return false
        if (this.data == null)
        {
            return false;
        }
        // Otherwise, search for the node in the left or right subtree
        else if (data < this.data)
        {
            if (this.left == null)
            {
                return false;
            }
            else
            {
                return this.left.Search(data);
            }
        }
        else
        {
            if (this.right == null)
            {
                return false;
            }
            else
            {
                return this.right.Search(data);
            }
        }
    }

    // Method to delete a node from the tree
    public void Delete(int data)
    {
        // If the tree is empty, return
        if (this.data == null)
        {
            return;
        }
        // Otherwise, delete the node from the left or right subtree
        else if (data < this.data)
        {
            if (this.left != null)
            {
                this.left.Delete(data);
            }
        }
        else
        {
            if (this.right != null)
            {
                this.right.Delete(data);
            }
        }
    }

    // Method to print the tree in preorder traversal
    public void PreorderTraversal()
    {
        // Print the data of the node
        Console.Write(this.data + " ");

        // Recursively print the left and right subtrees
        if (this.left != null)
        {
            this.left.PreorderTraversal();
        }
        if (this.right != null)
        {
            this.right.PreorderTraversal();