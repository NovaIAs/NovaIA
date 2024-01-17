```java
import java.util.*;
import java.lang.*;
import java.io.*;

// A class to represent a node in an adjacency list
class Node {
    int vertex;
    int weight;

    Node(int vertex, int weight) {
        this.vertex = vertex;
        this.weight = weight;
    }
}

// A class to represent a graph as an adjacency list
class Graph {
    // An array of ArrayLists to store the adjacency list
    private ArrayList<Node>[] adjList;

    // The number of vertices in the graph
    private int numVertices;

    // Constructor for the Graph class
    Graph(int numVertices) {
        this.numVertices = numVertices;

        // Create an array of ArrayLists to store the adjacency list
        adjList = new ArrayList[numVertices];

        // Initialize each ArrayList in the adjacency list
        for (int i = 0; i < numVertices; i++) {
            adjList[i] = new ArrayList<>();
        }
    }

    // Function to add an edge to the graph
    void addEdge(int src, int dest, int weight) {
        // Add an edge from src to dest with the given weight
        adjList[src].add(new Node(dest, weight));

        // Add an edge from dest to src with the same weight
        // (if the graph is undirected)
        adjList[dest].add(new Node(src, weight));
    }

    // Function to perform a depth-first search on the graph starting from the given vertex
    void DFS(int start) {
        // Create a stack to store the vertices that have been visited
        Stack<Integer> stack = new Stack<>();

        // Create a boolean array to track which vertices have been visited
        boolean[] visited = new boolean[numVertices];

        // Push the start vertex onto the stack
        stack.push(start);

        // While the stack is not empty, pop the top vertex and visit it
        while (!stack.isEmpty()) {
            int current = stack.pop();

            // If the current vertex has not been visited, visit it and push all its neighbors onto the stack
            if (!visited[current]) {
                visited[current] = true;
                System.out.println(current);

                for (Node neighbor : adjList[current]) {
                    if (!visited[neighbor.vertex]) {
                        stack.push(neighbor.vertex);
                    }
                }
            }
        }
    }

    // Function to perform a breadth-first search on the graph starting from the given vertex
    void BFS(int start) {
        // Create a queue to store the vertices that have been visited
        Queue<Integer> queue = new LinkedList<>();

        // Create a boolean array to track which vertices have been visited
        boolean[] visited = new boolean[numVertices];

        // Enqueue the start vertex
        queue.add(start);

        // While the queue is not empty, dequeue the front vertex and visit it
        while (!queue.isEmpty()) {
            int current = queue.remove();

            // If the current vertex has not been visited, visit it and enqueue all its neighbors
            if (!visited[current]) {
                visited[current] = true;
                System.out.println(current);

                for (Node neighbor : adjList[current]) {
                    if (!visited[neighbor.vertex]) {
                        queue.add(neighbor.vertex);
                    }
                }
            }
        }
    }

    // Function to find the shortest path between two vertices in the graph using Dijkstra's algorithm
    int[] Dijkstra(int start, int end) {
        // Create an array to store the shortest distances to each vertex from the start vertex
        int[] distances = new int[numVertices];

        // Initialize the distances to infinity
        for (int i = 0; i < numVertices; i++) {
            distances[i] = Integer.MAX_VALUE;
        }

        // Set the distance to the start vertex to 0
        distances[start] = 0;

        // Create a priority queue to store the vertices that have been visited
        PriorityQueue<Node> pq = new PriorityQueue<>((a, b) -> a.weight - b.weight);

        // Enqueue the start vertex
        pq.add(new Node(start, 0));

        // While the priority queue is not empty, dequeue the front vertex and visit it
        while (!pq.isEmpty()) {
            Node current = pq.remove();

            // If the current vertex is the end vertex, return the distances array
            if (current.vertex == end) {
                return distances;
            }

            // For each neighbor of the current vertex, update