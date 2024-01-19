```kotlin
// Import required libraries
import java.util.*
import kotlin.collections.HashMap

// Define a class to represent a Graph
class Graph {
    // Adjacency list to represent the graph
    private val adjList = HashMap<String, MutableList<String>>()

    // Function to add an edge to the graph
    fun addEdge(source: String, destination: String) {
        // If the adjacency list does not contain the source vertex, create it
        if (!adjList.containsKey(source)) {
            adjList[source] = mutableListOf()
        }

        // Add the destination vertex to the adjacency list of the source vertex
        adjList[source]?.add(destination)
    }

    // Function to perform Depth-First Search on the graph
    fun depthFirstSearch(startVertex: String): List<String> {
        // Create a stack to keep track of the vertices to be visited
        val stack = Stack<String>()

        // Create a set to keep track of the visited vertices
        val visited = HashSet<String>()

        // Create a list to store the traversal order
        val traversalOrder = mutableListOf<String>()

        // Push the start vertex onto the stack
        stack.push(startVertex)

        // While the stack is not empty
        while (stack.isNotEmpty()) {
            // Pop the top vertex from the stack
            val currentVertex = stack.pop()

            // If the current vertex has not been visited, mark it as visited and add it to the traversal order
            if (!visited.contains(currentVertex)) {
                visited.add(currentVertex)
                traversalOrder.add(currentVertex)

                // Push the adjacent vertices of the current vertex onto the stack
                adjList[currentVertex]?.forEach { adjacentVertex ->
                    if (!visited.contains(adjacentVertex)) {
                        stack.push(adjacentVertex)
                    }
                }
            }
        }

        // Return the traversal order
        return traversalOrder
    }

    // Function to perform Breadth-First Search on the graph
    fun breadthFirstSearch(startVertex: String): List<String> {
        // Create a queue to keep track of the vertices to be visited
        val queue = LinkedList<String>()

        // Create a set to keep track of the visited vertices
        val visited = HashSet<String>()

        // Create a list to store the traversal order
        val traversalOrder = mutableListOf<String>()

        // Enqueue the start vertex
        queue.add(startVertex)

        // While the queue is not empty
        while (queue.isNotEmpty()) {
            // Dequeue the front vertex from the queue
            val currentVertex = queue.poll()

            // If the current vertex has not been visited, mark it as visited and add it to the traversal order
            if (!visited.contains(currentVertex)) {
                visited.add(currentVertex)
                traversalOrder.add(currentVertex)

                // Enqueue the adjacent vertices of the current vertex
                adjList[currentVertex]?.forEach { adjacentVertex ->
                    if (!visited.contains(adjacentVertex)) {
                        queue.add(adjacentVertex)
                    }
                }
            }
        }

        // Return the traversal order
        return traversalOrder
    }

    // Function to find the shortest path between two vertices using Dijkstra's algorithm
    fun dijkstra(startVertex: String, endVertex: String): List<String> {
        // Create a map to store the shortest distances from the start vertex to all other vertices
        val distances = HashMap<String, Int>()

        // Create a set to keep track of the visited vertices
        val visited = HashSet<String>()

        // Initialize the distances to all vertices as infinity
        adjList.keys.forEach { vertex ->
            distances[vertex] = Int.MAX_VALUE
        }

        // Set the distance to the start vertex to 0
        distances[startVertex] = 0

        // While there are still unvisited vertices
        while (visited.size < adjList.size) {
            // Find the unvisited vertex with the smallest distance
            var minVertex: String? = null
            var minDistance = Int.MAX_VALUE
            for (vertex in adjList.keys) {
                if (!visited.contains(vertex) && distances[vertex]!! < minDistance) {
                    minVertex = vertex
                    minDistance = distances[vertex]!!
                }
            }

            // If no unvisited vertex with a finite distance is found, break the loop
            if (minVertex == null) {
                break
            }

            // Mark the current vertex as visited
            visited.add(minVertex)

            // Update the distances to the adjacent vertices
            adjList[minVertex]?.forEach { adjacentVertex ->
                if (!visited.contains(adjacentVertex)) {
                    val newDistance = distances[minVertex]!! + 1
                    if (newDistance < distances[adjacentVertex]!!) {
                        distances[adjacentVertex] = newDistance
                    }
                }
            }
        }

        // If the distance to the end vertex is still infinity, there is no path between the start and end vertices
        if (distances[endVertex] == Int.MAX_VALUE) {
            return emptyList()
        }

        // Reconstruct the shortest path from the start vertex to the end vertex
        val shortestPath = mutableListOf<String>()
        var currentVertex = endVertex
        while (currentVertex != startVertex) {
            shortestPath.add(0, currentVertex)
            for (vertex in adjList.keys) {
                if (distances[vertex]!! + 1 == distances[currentVertex]!!