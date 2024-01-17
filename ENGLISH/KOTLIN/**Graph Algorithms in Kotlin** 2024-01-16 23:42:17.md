```kotlin
// Define a class to represent a graph
class Graph {
    // The graph is represented as a map of vertices to their adjacent vertices
    private val adjacencyList: MutableMap<String, MutableSet<String>> = mutableMapOf()

    // Add a vertex to the graph
    fun addVertex(vertex: String) {
        adjacencyList[vertex] = mutableSetOf()
    }

    // Add an edge between two vertices
    fun addEdge(vertex1: String, vertex2: String) {
        adjacencyList[vertex1]?.add(vertex2)
        adjacencyList[vertex2]?.add(vertex1)
    }

    // Perform a depth-first search on the graph
    fun depthFirstSearch(startVertex: String): List<String> {
        val visitedVertices = mutableSetOf<String>()
        val result = mutableListOf<String>()
        depthFirstSearchRecursive(startVertex, visitedVertices, result)
        return result
    }

    // Recursive helper function for depth-first search
    private fun depthFirstSearchRecursive(
        currentVertex: String,
        visitedVertices: MutableSet<String>,
        result: MutableList<String>
    ) {
        visitedVertices.add(currentVertex)
        result.add(currentVertex)
        adjacencyList[currentVertex]?.forEach { adjacentVertex ->
            if (adjacentVertex !in visitedVertices) {
                depthFirstSearchRecursive(adjacentVertex, visitedVertices, result)
            }
        }
    }

    // Perform a breadth-first search on the graph
    fun breadthFirstSearch(startVertex: String): List<String> {
        val visitedVertices = mutableSetOf<String>()
        val result = mutableListOf<String>()
        val queue = ArrayDeque<String>()
        queue.add(startVertex)

        while (queue.isNotEmpty()) {
            val currentVertex = queue.removeFirst()
            visitedVertices.add(currentVertex)
            result.add(currentVertex)
            adjacencyList[currentVertex]?.forEach { adjacentVertex ->
                if (adjacentVertex !in visitedVertices) {
                    queue.addLast(adjacentVertex)
                }
            }
        }

        return result
    }

    // Find the shortest path between two vertices using Dijkstra's algorithm
    fun dijkstra(startVertex: String, endVertex: String): List<String> {
        // Initialize distances and predecessors for all vertices
        val distances = mutableMapOf<String, Int>()
        val predecessors = mutableMapOf<String, String>()
        adjacencyList.keys.forEach { vertex ->
            distances[vertex] = Int.MAX_VALUE
            predecessors[vertex] = ""
        }
        distances[startVertex] = 0

        // Use a priority queue to keep track of vertices to visit, ordered by their distance from the start vertex
        val pq = PriorityQueue<String>(Comparator { v1, v2 -> distances[v1]!! - distances[v2]!! })
        pq.add(startVertex)

        // While there are still vertices to visit
        while (pq.isNotEmpty()) {
            val currentVertex = pq.poll()

            // If the current vertex is the end vertex, we have found the shortest path
            if (currentVertex == endVertex) {
                break
            }

            // For each adjacent vertex
            adjacencyList[currentVertex]?.forEach { adjacentVertex ->
                // Calculate the distance to the adjacent vertex
                val newDistance = distances[currentVertex]!! + 1

                // If the new distance is shorter than the current distance, update the distance and predecessor
                if (newDistance < distances[adjacentVertex]!!) {
                    distances[adjacentVertex] = newDistance
                    predecessors[adjacentVertex] = currentVertex
                    pq.add(adjacentVertex)
                }
            }
        }

        // Reconstruct the shortest path from the end vertex to the start vertex
        val path = mutableListOf<String>()
        var currentVertex = endVertex
        while (currentVertex != startVertex) {
            path.add(currentVertex)
            currentVertex = predecessors[currentVertex]!!
        }
        path.add(startVertex)
        path.reverse()

        return path
    }

    // Find all paths between two vertices using a recursive backtracking algorithm
    fun findAllPaths(startVertex: String, endVertex: String): List<List<String>> {
        val paths = mutableListOf<List<String>>()
        val visitedVertices = mutableSetOf<String>()
        val currentPath = mutableListOf<String>()
        findAllPathsRecursive(startVertex, endVertex, visitedVertices, currentPath, paths)
        return paths
    }

    // Recursive helper function for finding all paths
    private fun findAllPathsRecursive(
        currentVertex: String,
        endVertex: String,
        visitedVertices: MutableSet<String>,
        currentPath: MutableList<String>,
        paths: MutableList<List<String>>
    ) {
        visitedVertices.add(currentVertex)
        currentPath.add(currentVertex)

        if (currentVertex == endVertex) {
            paths.add(currentPath.toList())
        } else {
            adjacencyList[currentVertex]?.forEach { adjacentVertex ->
                if (adjacentVertex !in visitedVertices) {
                    findAllPathsRecursive(adjacentVertex, endVertex, visitedVertices, currentPath, paths)
                }
            }
        }

        visitedVertices.remove(currentVertex)
        currentPath.removeLast()
    }
}

// Example usage
val graph = Graph()
graph.addVertex("A")
graph.addVertex("B")
graph.addVertex("C")
graph.addVertex("D")
graph.addVertex("E")
graph.addVertex("F")
graph.addEdge("A", "B")
graph.addEdge("A", "C")
graph.addEdge("B", "D")
graph.addEdge("C", "E")
graph.addEdge("D", "F")
graph.addEdge("E", "F")

println(graph.depthFirstSearch("A")) // [A, B, D, F, E, C]
println(graph.breadthFirstSearch("A")) // [A, B, C, D, E, F]
println(graph.dijkstra("A", "F")) // [A, B, D, F]
println(graph.findAllPaths("A", "F")) // [[A, B, D, F], [A, C, E, F]]
```