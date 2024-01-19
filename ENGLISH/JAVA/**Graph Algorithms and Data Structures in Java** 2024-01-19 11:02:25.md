```java
import java.util.*;
import java.io.*;

public class ComplexJavaCode {

    // Define a class to represent a graph
    static class Graph {
        // Adjacency list representation of the graph
        private Map<Integer, List<Integer>> adjList;
        // Number of vertices in the graph
        private int numVertices;

        // Constructor to create a graph with the given number of vertices
        public Graph(int numVertices) {
            this.numVertices = numVertices;
            adjList = new HashMap<>();
            for (int i = 1; i <= numVertices; i++) {
                adjList.put(i, new ArrayList<>());
            }
        }

        // Method to add an edge to the graph
        public void addEdge(int src, int dest) {
            adjList.get(src).add(dest);
            adjList.get(dest).add(src);
        }

        // Method to perform a depth-first search (DFS) on the graph
        public Set<Integer> DFS(int startVertex) {
            Set<Integer> visited = new HashSet<>();
            Stack<Integer> stack = new Stack<>();
            stack.push(startVertex);

            while (!stack.isEmpty()) {
                int currentVertex = stack.pop();
                if (!visited.contains(currentVertex)) {
                    visited.add(currentVertex);
                    for (int neighbor : adjList.get(currentVertex)) {
                        if (!visited.contains(neighbor)) {
                            stack.push(neighbor);
                        }
                    }
                }
            }

            return visited;
        }

        // Method to perform a breadth-first search (BFS) on the graph
        public Set<Integer> BFS(int startVertex) {
            Set<Integer> visited = new HashSet<>();
            Queue<Integer> queue = new LinkedList<>();
            queue.add(startVertex);

            while (!queue.isEmpty()) {
                int currentVertex = queue.remove();
                if (!visited.contains(currentVertex)) {
                    visited.add(currentVertex);
                    for (int neighbor : adjList.get(currentVertex)) {
                        if (!visited.contains(neighbor)) {
                            queue.add(neighbor);
                        }
                    }
                }
            }

            return visited;
        }

        // Method to find the shortest path between two vertices using Dijkstra's algorithm
        public Map<Integer, Integer> Dijkstra(int startVertex) {
            // Initialize distances to all vertices as infinity
            Map<Integer, Integer> distances = new HashMap<>();
            for (int i = 1; i <= numVertices; i++) {
                distances.put(i, Integer.MAX_VALUE);
            }
            distances.put(startVertex, 0);

            // Create a priority queue to store vertices based on their distances
            PriorityQueue<Integer> pq = new PriorityQueue<>((a, b) -> distances.get(a) - distances.get(b));
            pq.add(startVertex);

            while (!pq.isEmpty()) {
                int currentVertex = pq.remove();

                for (int neighbor : adjList.get(currentVertex)) {
                    int newDistance = distances.get(currentVertex) + 1;
                    if (newDistance < distances.get(neighbor)) {
                        distances.put(neighbor, newDistance);
                        pq.add(neighbor);
                    }
                }
            }

            return distances;
        }

        // Method to find the minimum spanning tree using Kruskal's algorithm
        public Set<Edge> KruskalMST() {
            // Sort edges by weight in ascending order
            List<Edge> edges = new ArrayList<>();
            for (int i = 1; i <= numVertices; i++) {
                for (int j = i + 1; j <= numVertices; j++) {
                    if (adjList.get(i).contains(j)) {
                        edges.add(new Edge(i, j, 1));
                    }
                }
            }
            Collections.sort(edges, (a, b) -> a.weight - b.weight);

            // Create a disjoint-set data structure to keep track of connected components
            DisjointSetUnion dsu = new DisjointSetUnion(numVertices);

            // Initialize the minimum spanning tree
            Set<Edge> mst = new HashSet<>();

            // Iterate over the edges in sorted order
            for (Edge edge : edges) {
                int root1 = dsu.find(edge.src);
                int root2 = dsu.find(edge.dest);

                // If the vertices are in different connected components, add the edge to the MST
                if (root1 != root2) {
                    mst.add(edge);
                    dsu.union(root1, root2);
                }
            }

            return mst;
        }
    }

    // Define a class to represent an edge
    static class Edge {
        int src;
        int dest;
        int weight;

        public Edge(int src, int dest, int weight) {
            this.src = src;
            this.dest = dest;
            this.weight = weight;
        }
    }

    // Define a class to represent a disjoint-set data structure
    static class DisjointSetUnion {
        int[] parent;
        int[] size;

        public DisjointSetUnion(int n) {
            parent = new int[n + 1];
            size = new int[n + 1];
            for (int i = 1; i <= n; i++) {
                parent[i] = i;
                size[i] = 1;
            }
        }

        public int find(int x) {
            if (x == parent[x]) {
                return x;
            }
            return parent[x] = find(parent[x]);
        }

        public void union(int x, int y) {
            int root1 = find(x);
            int root2 = find(y);

            // Merge the two sets by attaching the smaller tree to the larger tree
            if (size[root1] < size[root2]) {
                parent[root1] = root2;
                size[root2] += size[root1];
            } else {
                parent[root2] = root1;
                size[root1] += size[root2];
            }
        }
    }

    public static void main(String[] args) {
        // Create a graph with 10 vertices
        Graph graph = new Graph(10);

        // Add edges to the graph
        graph.addEdge(1, 2);
        graph.addEdge(1, 3);
        graph.addEdge(2, 4);
        graph.addEdge(2, 5);
        graph.addEdge(3, 6);
        graph.addEdge(3, 7);
        graph.addEdge(4, 8);
        graph.addEdge(4, 9);
        graph.addEdge(5, 10);

        // Perform depth-first search (DFS) on the graph
        System.out.println("DFS: " + graph.DFS(1));

        // Perform breadth-first search (BFS) on the graph
        System.out.println("BFS: " + graph.BFS(1));

        // Find the shortest path between vertices 1 and 10 using Dijkstra's algorithm
        System.out.println("Dijkstra's Shortest Path: " + graph.Dijkstra(1));

        // Find the minimum spanning tree using Kruskal's algorithm
        System.out.println("Kruskal's MST: " + graph.KruskalMST());
    }
}
```

This code demonstrates a variety of graph algorithms and data structures. It includes:

* **Depth-first search (DFS)**: A recursive algorithm that visits all vertices in a graph starting from a given vertex.
* **Breadth-first search (BFS)**: An iterative algorithm that visits all vertices in a graph level by level, starting from a given vertex.
* **Dijkstra's algorithm**: An algorithm that finds the shortest path from a given vertex to all other vertices in a weighted graph.
* **Kruskal's algorithm**: An algorithm that finds the minimum spanning tree of a weighted graph.
* **Disjoint-set data structure**: A data structure that represents a collection of disjoint sets. It supports operations such as finding the representative of a set and merging two sets.

The code uses the Java Collections Framework to represent the graph and its vertices and edges. It also uses a priority queue to implement Dijkstra's algorithm and a disjoint-set data structure to implement Kruskal's algorithm.

Overall, this code is a good example of how to implement a variety of graph algorithms in Java. It is well-structured and easy to follow, even for beginners.