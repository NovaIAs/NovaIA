```kotlin
class PolymorphicDataStructures {

    // Sealed class representing a node in a tree.
    sealed class TreeNode<T> {
        // Data class representing a leaf node.
        data class Leaf<T>(val value: T) : TreeNode<T>()

        // Data class representing a non-leaf node.
        data class Node<T>(val children: List<TreeNode<T>>) : TreeNode<T>()
    }

    // Function to build a binary tree from a list of integers.
    fun buildBinaryTree(values: List<Int>): TreeNode<Int> {
        // Create the root node with the first value.
        val root = TreeNode.Node(listOf(TreeNode.Leaf(values[0])))

        // Iterate over the remaining values.
        for (i in 1 until values.size) {
            // Add a new leaf node to the tree.
            val leaf = TreeNode.Leaf(values[i])

            // Find the appropriate position to insert the leaf node.
            var currentNode = root
            while (true) {
                when (currentNode) {
                    is TreeNode.Leaf -> {
                        // Reached a leaf node. Insert the new leaf here.
                        currentNode = TreeNode.Node(listOf(currentNode, leaf))
                        break
                    }
                    is TreeNode.Node -> {
                        // Reached a non-leaf node. Choose the left or right child to insert the new leaf.
                        val child = if (values[i] < currentNode.children[0].value) {
                            currentNode.children[0]
                        } else {
                            currentNode.children[1]
                        }

                        // Recursively search for the appropriate position to insert the new leaf.
                        currentNode = child
                    }
                }
            }
        }

        // Return the root of the binary tree.
        return root
    }

    // Function to print the tree in a human-readable format.
    fun printTree(tree: TreeNode<Int>) {
        // Helper function to print the tree recursively.
        fun printTreeHelper(node: TreeNode<Int>, level: Int) {
            // Print the value of the current node.
            println("${" ".repeat(level)}${node.value}")

            // Print the children of the current node.
            if (node is TreeNode.Node) {
                for (child in node.children) {
                    printTreeHelper(child, level + 1)
                }
            }
        }

        // Print the tree starting from the root node.
        printTreeHelper(tree, 0)
    }

    // Function to test the binary tree construction and printing.
    fun testBinaryTree() {
        // Create a list of integers.
        val values = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9)

        // Build a binary tree from the list.
        val tree = buildBinaryTree(values)

        // Print the binary tree.
        printTree(tree)
    }

    fun main(args: Array<String>) {
        testBinaryTree()
    }
}
```

Explanation:

1. Sealed Class `TreeNode`: This is a sealed class that represents the nodes in a tree. It has two subclasses:
   - `Leaf`: Represents a leaf node with a single value.
   - `Node`: Represents a non-leaf node with a list of children nodes.

2. Function `buildBinaryTree`: This function takes a list of integers and builds a binary tree. It starts with the first value as the root node and recursively adds the remaining values to the appropriate positions in the tree, maintaining the binary tree property.

3. Function `printTree`: This function prints the tree in a human-readable format. It uses a recursive helper function to traverse the tree and print the values of the nodes.

4. Function `testBinaryTree`: This function creates a list of integers, builds a binary tree from the list, and prints the tree.

5. Main Function: The entry point of the program. It calls the `testBinaryTree` function to test the binary tree construction and printing.

This code showcases a more advanced use of Kotlin, involving sealed classes, recursion, and tree data structures. It demonstrates how to build and print a binary tree from a list of integers.