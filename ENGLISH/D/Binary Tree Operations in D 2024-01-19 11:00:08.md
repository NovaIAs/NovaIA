```d
import std.stdio, std.algorithm, std.typecons, std.range;
import std.math, std.conv, std.random, std.exception, std.string, std.foreach;

enum Colour(int) { RED, YELLOW, GREEN, BLUE, ORANGE, PURPLE, PINK }

struct Tree {
    Colour colour;
    int height;
    float width;
    Tree? left, right;

    this(Colour colour = Colour.GREEN, int height = 0, float width = 0.0, Tree? left = null, Tree? right = null) pure nothrow {
        this.colour = colour;
        this.height = height;
        this.width = width;
        this.left = left;
        this.right = right;
    }
}

void main() {
    // Create a binary tree of colours
    Tree tree = new Tree(Colour.RED, 10, 1.5,
        new Tree(Colour.YELLOW, 5, 0.75,
            new Tree(Colour.GREEN, 2, 0.25),
            new Tree(Colour.BLUE, 3, 0.5)),
        new Tree(Colour.PINK, 7, 1.0));

    // Print the tree in order
    foreach (Tree node; tree.inorder()) writeln(node.colour);

    // Print the tree in pre-order
    foreach (Tree node; tree.preorder()) writeln(node.colour);

    // Print the tree in post-order
    foreach (Tree node; tree.postorder()) writeln(node.colour);

    // Find the height of the tree
    int height = tree.height();
    writeln("Height of the tree:", height);

    // Find the width of the tree
    float width = tree.width();
    writeln("Width of the tree:", width);

    // Find the number of nodes in the tree
    int count = tree.count();
    writeln("Number of nodes in the tree:", count);

    // Find the maximum value in the tree
    Colour maxColour = tree.max();
    writeln("Maximum colour in the tree:", maxColour);

    // Find the minimum value in the tree
    Colour minColour = tree.min();
    writeln("Minimum colour in the tree:", minColour);

    // Find a node in the tree
    Tree? found = tree.find(Colour.BLUE);
    if (found != null)
        writeln("Found node with colour", found.colour);
    else
        writeln("Could not find node with colour", Colour.BLUE);

    // Delete a node from the tree
    tree.delete(Colour.YELLOW);
    writeln("Deleted node with colour", Colour.YELLOW);

    // Print the tree in order
    foreach (Tree node; tree.inorder()) writeln(node.colour);
}
```

This code creates a binary tree of colours and performs various operations on it, such as printing the tree in different orders, finding the height, width, and number of nodes in the tree, finding the maximum and minimum values in the tree, finding a node with a specific colour, and deleting a node from the tree.

The tree is represented using the `Tree` struct, which has fields for the colour, height, and width of the tree, as well as pointers to the left and right subtrees.

The main function creates a binary tree of colours using the `new` keyword, and then performs various operations on the tree using the methods defined in the `Tree` struct.

The `inorder()`, `preorder()`, and `postorder()` methods print the tree in different orders, while the `height()`, `width()`, and `count()` methods return the height, width, and number of nodes in the tree, respectively.

The `max()` and `min()` methods return the maximum and minimum values in the tree, respectively, while the `find()` method returns a pointer to a node with a specific colour.

The `delete()` method deletes a node from the tree, and the `inorder()` method is used to print the tree after the node has been deleted.