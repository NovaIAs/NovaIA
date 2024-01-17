```groovy
// Define a class to represent a graph
class Graph {

    // Adjacency list to store the graph
    private Map<String, List<String>> adjacencyList

    // Constructor to initialize the graph
    Graph() {
        adjacencyList = new HashMap<>()
    }

    // Method to add a vertex to the graph
    void addVertex(String vertex) {
        if (!adjacencyList.containsKey(vertex)) {
            adjacencyList.put(vertex, new ArrayList<>())
        }
    }

    // Method to add an edge to the graph
    void addEdge(String source, String destination) {
        if (!adjacencyList.containsKey(source)) {
            addVertex(source)
        }
        if (!adjacencyList.containsKey(destination)) {
            addVertex(destination)
        }
        adjacencyList.get(source).add(destination)
    }

    // Method to perform depth-first search on the graph
    List<String> depthFirstSearch(String startVertex) {
        // Create a stack to store the vertices to be visited
        Stack<String> stack = new Stack<>()

        // Create a set to keep track of visited vertices
        Set<String> visited = new HashSet<>()

        // Create a list to store the traversal order
        List<String> traversalOrder = new ArrayList<>()

        // Push the start vertex onto the stack
        stack.push(startVertex)

        // While the stack is not empty
        while (!stack.empty()) {
            // Pop the top vertex from the stack
            String currentVertex = stack.pop()

            // If the vertex has not been visited, visit it
            if (!visited.contains(currentVertex)) {
                // Add the vertex to the traversal order
                traversalOrder.add(currentVertex)

                // Mark the vertex as visited
                visited.add(currentVertex)

                // Push the neighbors of the vertex onto the stack
                for (String neighbor : adjacencyList.get(currentVertex)) {
                    stack.push(neighbor)
                }
            }
        }

        // Return the traversal order
        return traversalOrder
    }

    // Method to perform breadth-first search on the graph
    List<String> breadthFirstSearch(String startVertex) {
        // Create a queue to store the vertices to be visited
        Queue<String> queue = new LinkedList<>()

        // Create a set to keep track of visited vertices
        Set<String> visited = new HashSet<>()

        // Create a list to store the traversal order
        List<String> traversalOrder = new ArrayList<>()

        // Add the start vertex to the queue
        queue.add(startVertex)

        // While the queue is not empty
        while (!queue.isEmpty()) {
            // Dequeue the front vertex from the queue
            String currentVertex = queue.remove()

            // If the vertex has not been visited, visit it
            if (!visited.contains(currentVertex)) {
                // Add the vertex to the traversal order
                traversalOrder.add(currentVertex)

                // Mark the vertex as visited
                visited.add(currentVertex)

                // Enqueue the neighbors of the vertex
                for (String neighbor : adjacencyList.get(currentVertex)) {
                    queue.add(neighbor)
                }
            }
        }

        // Return the traversal order
        return traversalOrder
    }

    // Method to check if the graph contains a cycle
    boolean hasCycle() {
        // Create a set to keep track of visited vertices
        Set<String> visited = new HashSet<>()

        // Create a set to keep track of vertices that are currently being visited
        Set<String> visiting = new HashSet<>()

        // Iterate over all the vertices in the graph
        for (String vertex : adjacencyList.keySet()) {
            // If the vertex has not been visited, perform a depth-first search starting from that vertex
            if (!visited.contains(vertex)) {
                if (hasCycleHelper(vertex, visited, visiting)) {
                    return true
                }
            }
        }

        // If no cycles are found, return false
        return false
    }

    // Helper method to perform depth-first search and check for cycles
    private boolean hasCycleHelper(String vertex, Set<String> visited, Set<String> visiting) {
        // If the vertex is currently being visited, there is a cycle
        if (visiting.contains(vertex)) {
            return true
        }

        // Mark the vertex as being visited
        visiting.add(vertex)

        // Iterate over the neighbors of the vertex
        for (String neighbor : adjacencyList.get(vertex)) {
            // If the neighbor has not been visited, perform a depth-first search starting from that neighbor
            if (!visited.contains(neighbor)) {
                if (hasCycleHelper(neighbor, visited, visiting)) {
                    return true
                }
            }
        }

        // Mark the vertex as visited
        visited.add(vertex)

        // Remove the vertex from the set of vertices that are currently being visited
        visiting.remove(vertex)

        // Return false if no cycles are found
        return false
    }

    // Method to find the shortest path between two vertices using Dijkstra's algorithm
    List<String> shortestPath(String startVertex, String endVertex) {
        // Create a map to store the distances from the start vertex to all other vertices
        Map<String, Integer> distances = new HashMap<>()

        // Create a map to store the previous vertices in the shortest path from the start vertex to all other vertices
        Map<String, String> previousVertices = new HashMap<>()

        // Initialize the distances of all vertices to infinity, except for the start vertex which is set to 0
        for (String vertex : adjacencyList.keySet()) {
            distances.put(vertex, Integer.MAX_VALUE)
        }
        distances.put(startVertex, 0)

        // Create a set to keep track of visited vertices
        Set<String> visited = new HashSet<>()

        // While there are still unvisited vertices
        while (visited.size() < adjacencyList.size()) {
            // Find the unvisited vertex with the smallest distance from the start vertex
            String currentVertex = null
            int minDistance = Integer.MAX_VALUE
            for (String vertex : adjacencyList.keySet()) {
                if (!visited.contains(vertex) && distances.get(vertex) < minDistance) {
                    currentVertex = vertex
                    minDistance = distances.get(vertex)
                }
            }

            // If no unvisited vertices are found, break out of the loop
            if (currentVertex == null) {
                break
            }

            // Visit the current vertex
            visited.add(currentVertex)

            // Relax the edges from the current vertex
            for (String neighbor : adjacencyList.get(currentVertex)) {
                int newDistance = distances.get(currentVertex) + 1
                if (newDistance < distances.get(neighbor)) {
                    distances.put(neighbor, newDistance)
                    previousVertices.put(neighbor, currentVertex)
                }
            }
        }

        // If the end vertex is not reachable from the start vertex, return an empty list
        if (distances.get(endVertex) == Integer.MAX_VALUE) {
            return []
        }

        // Reconstruct the shortest path from the end vertex to the start vertex
        List<String> shortestPath = new ArrayList<>()
        String currentVertex = endVertex
        while (currentVertex != startVertex) {
            shortestPath.add(0, currentVertex)
            currentVertex = previousVertices.get(currentVertex)
        }
        shortestPath.add(0, startVertex)

        // Return the shortest path
        return shortestPath
    }
}

// Create a graph
Graph graph = new Graph()

// Add vertices to the graph
graph.addVertex("A")
graph.addVertex("B")
graph.addVertex("C")
graph.addVertex("D")
graph.addVertex("E")
graph.addVertex("F")

// Add edges to the graph
graph.addEdge("A", "B")
graph.addEdge("A", "C")
graph.addEdge("B", "D")
graph.addEdge("C", "E")
graph.addEdge("D", "F")
graph.addEdge("E", "F")

// Perform depth-first search on the graph
List<String> traversalOrder = graph.depthFirstSearch("A")
println("Depth-first search traversal order: ${traversalOrder}")

// Perform breadth-first search on the graph
traversalOrder = graph.breadthFirstSearch("A")
println("Breadth-first search traversal order: ${traversalOrder}")

// Check if the graph contains a cycle
boolean hasCycle = graph.hasCycle()
println("Does the graph contain a cycle: ${hasCycle}")

// Find the shortest path between two vertices
List<String> shortestPath = graph.shortestPath("A", "F")
println("Shortest path between A and F: ${shortestPath}")
```

This code is a complete implementation of a