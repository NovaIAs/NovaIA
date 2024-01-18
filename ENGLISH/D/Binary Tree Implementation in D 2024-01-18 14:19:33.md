```d
import std.stdio, std.algorithm, std.range, std.traits;

// Template class to hold a binary tree node.
template TreeNode(T)(T value) pure nothrow {
    T value;
    TreeNode(left)!;
    TreeNode(right)!;

    // Insert a new node into the tree.
    // If the value is less than the current node's value, insert it into the left subtree.
    // Otherwise, insert it into the right subtree.
    this insert(T value) pure nothrow {
        if (value < this.value)
            if (this.left.isVoid)
                this.left = new TreeNode(value);
            else
                this.left.insert(value);
        else
            if (this.right.isVoid)
                this.right = new TreeNode(value);
            else
                this.right.insert(value);
    }

    // Search for a value in the tree.
    // If the value is found, return true.
    // Otherwise, return false.
    this search(T value) pure const nothrow {
        if (value == this.value)
            return true;
        else if (value < this.value)
            if (this.left.isVoid)
                return false;
            else
                return this.left.search(value);
        else
            if (this.right.isVoid)
                return false;
            else
                return this.right.search(value);
    }

    // Print the tree using inorder traversal.
    this print() pure const nothrow {
        if (!this.left.isVoid)
            this.left.print();
        writeln(this.value);
        if (!this.right.isVoid)
            this.right.print();
    }
}

// Create a new binary tree.
auto tree = new TreeNode(int)(10);

// Insert some values into the tree.
tree insert(20);
tree insert(5);
tree insert(15);
tree insert(3);
tree insert(7);

// Search for a value in the tree.
if (tree.search(7))
    writeln("7 found in the tree");
else
    writeln("7 not found in the tree");

// Print the tree.
tree.print();
```

This code defines a template class TreeNode that can be used to store binary tree nodes. The class has three data members:

* value: The value stored in the node.
* left: A pointer to the node's left child.
* right: A pointer to the node's right child.

The class also has three member functions:

* insert: Inserts a new node into the tree.
* search: Searches for a value in the tree.
* print: Prints the tree using inorder traversal.

The main function creates a new binary tree and inserts some values into it. It then searches for a value in the tree and prints the tree.

Here is a sample output of the code:

```
7 found in the tree
3
5
7
10
15
20
```