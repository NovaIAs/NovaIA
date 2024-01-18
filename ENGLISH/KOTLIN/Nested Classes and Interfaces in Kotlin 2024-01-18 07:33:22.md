```kotlin
// Kotlin code to demonstrate the use of nested classes
// and interfaces

// An interface to represent a Queue
interface Queue<T> {
    fun enqueue(element: T)
    fun dequeue(): T
    fun size(): Int
    fun isEmpty(): Boolean
}

// A class representing a linked list implementation of a Queue
class LinkedListQueue<T> : Queue<T> {

    // Inner class to represent a node in the linked list
    inner class Node<T>(var value: T, var next: Node<T>? = null)

    // Head and tail of the queue
    private var head: Node<T>? = null
    private var tail: Node<T>? = null

    // Function to add an element to the queue
    override fun enqueue(element: T) {
        val newNode = Node(element)
        if (head == null) {
            head = newNode
        } else {
            tail?.next = newNode
        }
        tail = newNode
    }

    // Function to remove an element from the queue
    override fun dequeue(): T {
        val value = head?.value
        head = head?.next
        if (head == null) {
            tail = null
        }
        return value!!
    }

    // Function to get the size of the queue
    override fun size(): Int {
        var count = 0
        var current = head
        while (current != null) {
            count++
            current = current.next
        }
        return count
    }

    // Function to check if the queue is empty
    override fun isEmpty(): Boolean {
        return head == null
    }
}

// A class representing a Binary Tree
class BinaryTree<T> {

    // Inner class to represent a node in the binary tree
    inner class Node<T>(var value: T, var left: Node<T>? = null, var right: Node<T>? = null)

    // Root of the binary tree
    private var root: Node<T>? = null

    // Function to insert a value into the binary tree
    fun insert(value: T) {
        val newNode = Node(value)
        if (root == null) {
            root = newNode
        } else {
            var current = root
            while (true) {
                if (value < current?.value) {
                    if (current?.left == null) {
                        current?.left = newNode
                        break
                    } else {
                        current = current?.left
                    }
                } else {
                    if (current?.right == null) {
                        current?.right = newNode
                        break
                    } else {
                        current = current?.right
                    }
                }
            }
        }
    }

    // Function to perform a breadth-first traversal of the binary tree
    fun breadthFirstTraversal() {
        val queue = LinkedListQueue<Node<T>>()
        queue.enqueue(root)

        while (!queue.isEmpty()) {
            val current = queue.dequeue()
            println(current.value)

            if (current.left != null) {
                queue.enqueue(current.left)
            }
            if (current.right != null) {
                queue.enqueue(current.right)
            }
        }
    }
}

// A class representing a Graph
class Graph<T> {

    // Inner class to represent a vertex in the graph
    inner class Vertex<T>(var value: T)

    // Inner class to represent an edge in the graph
    inner class Edge<T>(var from: Vertex<T>, var to: Vertex<T>, var weight: Int)

    // Map to store the vertices of the graph
    private val vertices = mutableMapOf<T, Vertex<T>>()

    // Map to store the edges of the graph
    private val edges = mutableSetOf<Edge<T>>()

    // Function to add a vertex to the graph
    fun addVertex(value: T) {
        val vertex = Vertex(value)
        vertices[value] = vertex
    }

    // Function to add an edge to the graph
    fun addEdge(from: T, to: T, weight: Int) {
        val fromVertex = vertices[from]
        val toVertex = vertices[to]
        if (fromVertex != null && toVertex != null) {
            val edge = Edge(fromVertex, toVertex, weight)
            edges.add(edge)
        }
    }

    // Function to perform a depth-first traversal of the graph
    fun depthFirstTraversal(start: T) {
        val visited = mutableSetOf<Vertex<T>>()
        val stack = mutableListOf<Vertex<T>>()
        stack.push(vertices[start]!!)

        while (stack.isNotEmpty()) {
            val current = stack.pop()
            if (current !in visited) {
                println(current.value)
                visited.add(current)
                for (edge in edges) {
                    if (edge.from == current) {
                        stack.push(edge.to)