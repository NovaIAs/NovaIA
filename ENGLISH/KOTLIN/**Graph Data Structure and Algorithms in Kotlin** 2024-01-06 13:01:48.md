```kotlin
// Import necessary libraries
import java.util.*
import kotlin.collections.HashMap

// Define a class to represent a graph
class Graph<T> {

    // A map to store the vertices and their adjacent vertices
    private val vertices: HashMap<T, MutableList<T>> = HashMap()

    // A method to add a vertex to the graph
    fun addVertex(vertex: T) {
        vertices[vertex] = mutableListOf()
    }

    // A method to add an edge between two vertices
    fun addEdge(vertex1: T, vertex2: T) {
        vertices[vertex1]?.add(vertex2)
        vertices[vertex2]?.add(vertex1)
    }

    // A method to perform a depth-first search on the graph
    fun depthFirstSearch(startVertex: T): List<T> {
        val visitedVertices: MutableList<T> = mutableListOf()
        val stack: Stack<T> = Stack()

        stack.push(startVertex)

        while (stack.isNotEmpty()) {
            val currentVertex = stack.pop()

            if (currentVertex !in visitedVertices) {
                visitedVertices.add(currentVertex)

                for (adjacentVertex in vertices[currentVertex]!!) {
                    if (adjacentVertex !in visitedVertices) {
                        stack.push(adjacentVertex)
                    }
                }
            }
        }

        return visitedVertices
    }

    // A method to perform a breadth-first search on the graph
    fun breadthFirstSearch(startVertex: T): List<T> {
        val visitedVertices: MutableList<T> = mutableListOf()
        val queue: Queue<T> = LinkedList()

        queue.add(startVertex)

        while (queue.isNotEmpty()) {
            val currentVertex = queue.poll()

            if (currentVertex !in visitedVertices) {
                visitedVertices.add(currentVertex)

                for (adjacentVertex in vertices[currentVertex]!!) {
                    if (adjacentVertex !in visitedVertices) {
                        queue.add(adjacentVertex)
                    }
                }
            }
        }

        return visitedVertices
    }

    // A method to find the shortest path between two vertices using Dijkstra's algorithm
    fun dijkstra(startVertex: T, endVertex: T): List<T> {
        val distances: HashMap<T, Int> = HashMap()
        val previousVertices: HashMap<T, T> = HashMap()

        for (vertex in vertices.keys) {
            distances[vertex] = Int.MAX_VALUE
        }

        distances[startVertex] = 0

        val unvisitedVertices: PriorityQueue<T> = PriorityQueue(compareBy { distances[it] })
        unvisitedVertices.add(startVertex)

        while (unvisitedVertices.isNotEmpty()) {
            val currentVertex = unvisitedVertices.poll()

            if (currentVertex == endVertex) {
                break
            }

            for (adjacentVertex in vertices[currentVertex]!!) {
                val newDistance = distances[currentVertex]!! + 1

                if (newDistance < distances[adjacentVertex]!!) {
                    distances[adjacentVertex] = newDistance
                    previousVertices[adjacentVertex] = currentVertex
                    unvisitedVertices.add(adjacentVertex)
                }
            }
        }

        val shortestPath: MutableList<T> = mutableListOf()
        var currentVertex = endVertex

        while (currentVertex != startVertex) {
            shortestPath.add(0, currentVertex)
            currentVertex = previousVertices[currentVertex]!!
        }

        shortestPath.add(0, startVertex)

        return shortestPath
    }
}

// Create a graph
val graph = Graph<Char>()

// Add vertices to the graph
graph.addVertex('A')
graph.addVertex('B')
graph.addVertex('C')
graph.addVertex('D')
graph.addVertex('E')
graph.addVertex('F')

// Add edges to the graph
graph.addEdge('A', 'B')
graph.addEdge('A', 'C')
graph.addEdge('B', 'D')
graph.addEdge('C', 'E')
graph.addEdge('D', 'F')
graph.addEdge('E', 'F')

// Perform a depth-first search on the graph starting from vertex 'A'
println("Depth-first search:")
println(graph.depthFirstSearch('A'))

// Perform a breadth-first search on the graph starting from vertex 'A'
println("Breadth-first search:")
println(graph.breadthFirstSearch('A'))

// Find the shortest path between vertices 'A' and 'F' using Dijkstra's algorithm
println("Shortest path between 'A' and 'F':")
println(graph.dijkstra('A', 'F'))
```

This code implements a graph data structure in Kotlin. It includes methods for adding vertices and edges to the graph, as well as methods for performing depth-first search, breadth-first search, and finding the shortest path between two vertices using Dijkstra's algorithm.

Here's a breakdown of the code:

1. **Graph Class:**
   - We define a `Graph` class that represents a graph.
   - It has a `vertices` property, which is a `HashMap` that maps each vertex to a list of its adjacent vertices.

2. **Vertex and Edge Manipulation Methods:**
   - `addVertex` method is used to add a new vertex to the graph.
   - `addEdge` method is used to add an edge between two existing vertices in the graph.

3. **Graph Traversal Algorithms:**
   - **Depth-First Search (DFS):**
     - The `depthFirstSearch` method performs a depth-first search on the graph starting from a specified vertex.
     - It uses a stack to keep track of the vertices that have been visited.
     - It returns a list of the vertices in the order in which they were visited.
   - **Breadth-First Search (BFS):**
     - The `breadthFirstSearch` method performs a breadth-first search on the graph starting from a specified vertex.
     - It uses a queue to keep track of the vertices that have been visited.
     - It returns a list of the vertices in the order in which they were visited.

4. **Shortest Path Algorithm:**
   - **Dijkstra's Algorithm:**
     - The `dijkstra` method finds the shortest path between two vertices in the graph using Dijkstra's algorithm.
     - It returns a list of vertices that represents the shortest path between the two vertices.

5. **Example Usage:**
   - We create a graph object and add vertices and edges to it.
   - We then call the `depthFirstSearch`, `breadthFirstSearch`, and `dijkstra` methods to demonstrate their functionality.

This code showcases a comprehensive graph data structure implementation with various graph traversal and shortest path algorithms. It provides a solid foundation for working with graphs in Kotlin.