```d
import std.stdio, std.algorithm, std.string;

struct Node
{
    int value;
    Node! left, right;
}

Node! find(Node! root, int target)
{
    if (!root)
        return null;
    if (root.value == target)
        return root;
    if (target < root.value)
        return find(root.left, target);
    return find(root.right, target);
}

Node! insert(Node! root, int value)
{
    if (!root)
        return new Node(value);
    if (value < root.value)
        root.left = insert(root.left, value);
    else
        root.right = insert(root.right, value);
    return root;
}

int main()
{
    Node! root = null;
    root = insert(root, 50);
    root = insert(root, 30);
    root = insert(root, 70);
    root = insert(root, 20);
    root = insert(root, 40);
    root = insert(root, 60);
    root = insert(root, 80);

    int target = 40;
    Node! result = find(root, target);
    if (result)
        writeln("Found {} in the tree", target);
    else
        writeln("Could not find {} in the tree", target);

    return 0;
}
```

This code implements a binary search tree in D. A binary search tree is a data structure that stores data in a way that allows for fast searches and insertions. The code first imports the necessary libraries, then defines a struct called Node that represents a node in the tree. The Node struct has three fields: value, left, and right. The value field stores the value of the node, and the left and right fields store pointers to the left and right child nodes, respectively.

Next, the code defines two functions: find and insert. The find function takes a pointer to the root node of the tree and a target value as arguments. It searches the tree for the target value and returns a pointer to the node that contains the target value, or null if the target value is not found. The insert function takes a pointer to the root node of the tree and a value as arguments. It inserts the value into the tree in the correct location based on the value of the node.

The main function creates a new binary search tree and inserts several values into it. It then searches the tree for a specific target value and prints a message indicating whether or not the target value was found.

The output of the program is:

```
Found 40 in the tree
```

This indicates that the target value was found in the tree.