```kotlin
// Class to represent a binary tree node
class Node(var data: Int, var left: Node? = null, var right: Node? = null)

// Function to construct a binary tree from a list of values
// (assuming the values are in level order)
fun constructBinaryTree(values: List<Int?>): Node? {
    if (values.isEmpty()) {
        return null
    }

    // Create the root node with the first value in the list
    val root = Node(values[0]!!)

    // Create a queue to store the nodes that have been processed
    // but whose children have not yet been created
    val queue = ArrayDeque<Node>()
    queue.add(root)

    // Iterate over the remaining values in the list
    for (i in 1 until values.size) {
        // Get the next node from the queue
        val node = queue.removeFirst()

        // If the current value is not null, create the left child node
        // and add it to the queue
        if (values[i] != null) {
            node.left = Node(values[i]!!)
            queue.add(node.left!!)
        }

        // Increment i to get the next value in the list
        i++

        // If the current value is not null, create the right child node
        // and add it to the queue
        if (i < values.size && values[i] != null) {
            node.right = Node(values[i]!!)
            queue.add(node.right!!)
        }
    }

    // Return the root node of the constructed binary tree
    return root
}

// Function to perform a depth-first traversal of a binary tree
// and print the data of each node in pre-order (root, left, right)
fun preOrderTraversal(root: Node?) {
    if (root == null) {
        return
    }

    // Print the data of the current node
    print("${root.data} ")

    // Recursively traverse the left subtree
    preOrderTraversal(root.left)

    // Recursively traverse the right subtree
    preOrderTraversal(root.right)
}

// Function to perform a depth-first traversal of a binary tree
// and print the data of each node in in-order (left, root, right)
fun inOrderTraversal(root: Node?) {
    if (root == null) {
        return
    }

    // Recursively traverse the left subtree
    inOrderTraversal(root.left)

    // Print the data of the current node
    print("${root.data} ")

    // Recursively traverse the right subtree
    inOrderTraversal(root.right)
}

// Function to perform a depth-first traversal of a binary tree
// and print the data of each node in post-order (left, right, root)
fun postOrderTraversal(root: Node?) {
    if (root == null) {
        return
    }

    // Recursively traverse the left subtree
    postOrderTraversal(root.left)

    // Recursively traverse the right subtree
    postOrderTraversal(root.right)

    // Print the data of the current node
    print("${root.data} ")
}

// Function to find the height of a binary tree
fun height(root: Node?): Int {
    if (root == null) {
        return 0
    }

    // Calculate the height of the left subtree
    val leftHeight = height(root.left)

    // Calculate the height of the right subtree
    val rightHeight = height(root.right)

    // Return the maximum of the two heights plus one (for the current node)
    return maxOf(leftHeight, rightHeight) + 1
}

// Function to check if a binary tree is balanced
fun isBalanced(root: Node?): Boolean {
    if (root == null) {
        return true
    }

    // Calculate the height of the left subtree
    val leftHeight = height(root.left)

    // Calculate the height of the right subtree
    val rightHeight = height(root.right)

    // Check if the difference in height is greater than 1
    if (Math.abs(leftHeight - rightHeight) > 1) {
        return false
    }

    // Recursively check if the left and right subtrees are balanced
    return isBalanced(root.left) && isBalanced(root.right)
}

// Function to perform a breadth-first traversal of a binary tree
// and print the data of each node in level order
fun levelOrderTraversal(root: Node?) {
    if (root == null) {
        return
    }

    // Create a queue to store the nodes that have been processed
    // but whose children have not yet been visited
    val queue = ArrayDeque<Node>()
    queue.add(root)

    // While the queue is not empty
    while (queue.isNotEmpty()) {
        // Get the next node from the queue
        val node = queue.removeFirst()

        // Print the data of the current node
        print("${node.data} ")

        // If the left child of the current node is not null, add it to the queue
        if (node.left != null) {
            queue.add(node.left!!)
        }

        // If the right child of the current node is not null, add it to the queue
        if (node.right != null) {
            queue.add(node.right!!)
        }
    }
}

// Example usage
val values