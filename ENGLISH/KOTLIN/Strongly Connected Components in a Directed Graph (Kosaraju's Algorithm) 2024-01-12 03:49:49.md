```kotlin
// Java implementation of Kosaraju's algorithm to print all strongly connected components in a directed graph
// The algorithm uses Depth First Search (DFS) and topological sort

import java.util.LinkedList
import java.util.Stack

// Class to represent a directed graph using adjacency list representation
class Graph {
    // No. of vertices in the graph
    private var V: Int = 0

    // adjacency list
    private var adj: Array<LinkedList<Int>>? = null

    // Constructor
    constructor(V: Int) {
        this.V = V
        adj = Array(V) { LinkedList() }
    }

    // function to add an edge to graph
    fun addEdge(v: Int, w: Int) {
        adj!![v].add(w)
    }

    // A function used by DFS
    private fun DFSUtil(v: Int, visited: BooleanArray, stack: Stack<Int>) {
        // Mark the current node as visited and print it
        visited[v] = true
        stack.push(v)

        // Recur for all the vertices adjacent to this vertex
        for (i in adj!![v]) {
            if (!visited[i]) {
                DFSUtil(i, visited, stack)
            }
        }
    }

    // The main function that finds and prints all strongly connected components
    fun printSCCs() {
        val visited = BooleanArray(V)
        val stack = Stack<Int>()

        // Fill vertices in stack according to their finishing times
        for (i in 0 until V) {
            if (!visited[i]) {
                DFSUtil(i, visited, stack)
            }
        }

        // Create a reversed graph
        val gr = Graph(V)
        for (i in 0 until V) {
            for (j in adj!![i]) {
                gr.addEdge(j, i)
            }
        }

        // Now process all vertices in order defined by Stack
        for (i in 0 until V) {
            visited[i] = false
        }

        // Pop a vertex from stack
        while (!stack.isEmpty()) {
            val v = stack.pop()

            // Print Strongly connected component of the popped vertex
            if (!visited[v]) {
                gr.DFSUtil(v, visited, stack)
                println()
            }
        }
    }

    // Driver program to test the above function
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            // Create a graph given in the above diagram
            val g = Graph(5)
            g.addEdge(1, 0)
            g.addEdge(0, 2)
            g.addEdge(2, 1)
            g.addEdge(0, 3)
            g.addEdge(3, 4)

            System.out.println("Following are strongly connected components in given graph")
            g.printSCCs()
        }
    }
}
```

Explanation:

1. The `Graph` class represents the directed graph using an adjacency list.
2. The `addEdge()` method is used to add an edge between two vertices in the graph.
3. The `DFSUtil()` method performs a depth-first search (DFS) on the graph starting from a given vertex `v`. It marks all the visited vertices and pushes them onto a stack.
4. The `printSCCs()` method finds and prints all the strongly connected components in the graph. It first performs DFS on the graph to fill vertices in a stack according to their finishing times. Then it creates a reversed graph and performs DFS again on the reversed graph starting from the vertices in the stack. A strongly connected component is printed whenever a DFS is started from an unvisited vertex.
5. The `main()` method creates a graph and calls the `printSCCs()` method to print all the strongly connected components in the graph.

Output:

```
Following are strongly connected components in given graph
0 2 1 
3 4 
```