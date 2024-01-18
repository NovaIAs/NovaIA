```kotlin
// A custom data structure for representing a binary tree node
data class Node<T>(val value: T, var left: Node<T>? = null, var right: Node<T>? = null)

// A function that takes a binary tree as input and returns the sum of all the values
// in the tree
fun sumOfTree(root: Node<Int>?): Int {
    if (root == null) {
        return 0
    }

    return root.value + sumOfTree(root.left) + sumOfTree(root.right)
}

// A function that takes a binary tree as input and returns the height of the tree
fun heightOfTree(root: Node<Int>?): Int {
    if (root == null) {
        return 0
    }

    return 1 + maxOf(heightOfTree(root.left), heightOfTree(root.right))
}

// A function that takes a binary tree as input and returns a list of all the values
// in the tree in breadth-first order
fun breadthFirstSearch(root: Node<Int>?): List<Int> {
    val queue = ArrayDeque<Node<Int>>()
    val result = mutableListOf<Int>()

    if (root != null) {
        queue.addLast(root)
    }

    while (queue.isNotEmpty()) {
        val node = queue.removeFirst()

        result.add(node.value)

        if (node.left != null) {
            queue.addLast(node.left)
        }

        if (node.right != null) {
            queue.addLast(node.right)
        }
    }

    return result
}

// A function that takes a binary tree as input and returns a list of all the values
// in the tree in depth-first order (pre-order traversal)
fun depthFirstSearchPreOrder(root: Node<Int>?): List<Int> {
    val result = mutableListOf<Int>()

    fun traverse(node: Node<Int>?) {
        if (node != null) {
            result.add(node.value)
            traverse(node.left)
            traverse(node.right)
        }
    }

    traverse(root)

    return result
}

// A function that takes a binary tree as input and returns a list of all the values
// in the tree in depth-first order (in-order traversal)
fun depthFirstSearchInOrder(root: Node<Int>?): List<Int> {
    val result = mutableListOf<Int>()

    fun traverse(node: Node<Int>?) {
        if (node != null) {
            traverse(node.left)
            result.add(node.value)
            traverse(node.right)
        }
    }

    traverse(root)

    return result
}

// A function that takes a binary tree as input and returns a list of all the values
// in the tree in depth-first order (post-order traversal)
fun depthFirstSearchPostOrder(root: Node<Int>?): List<Int> {
    val result = mutableListOf<Int>()

    fun traverse(node: Node<Int>?) {
        if (node != null) {
            traverse(node.left)
            traverse(node.right)
            result.add(node.value)
        }
    }

    traverse(root)

    return result
}

// A function that takes a binary tree as input and returns a list of all the paths
// from the root node to each leaf node
fun allPathsFromRootToLeaf(root: Node<Int>?): List<List<Int>> {
    val result = mutableListOf<List<Int>>()

    fun traverse(node: Node<Int>?, path: List<Int>) {
        if (node == null) {
            return
        }

        val newPath = path + node.value

        if (node.left == null && node.right == null) {
            result.add(newPath)
            return
        }

        traverse(node.left, newPath)
        traverse(node.right, newPath)
    }

    traverse(root, emptyList())

    return result
}

// A function that takes a binary tree as input and returns the maximum value in the tree
fun maxOfTree(root: