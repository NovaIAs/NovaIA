```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <stack>
#include <queue>
#include <unordered_map>
#include <unordered_set>

using namespace std;

// A structure to represent a graph
struct Graph {
    // A map to store the adjacency list
    unordered_map<string, vector<string>> adjList;

    // A method to add a directed edge to the graph
    void addEdge(string u, string v) {
        adjList[u].push_back(v);
    }

    // A method to perform a depth-first search on the graph
    void DFS(string start, unordered_set<string>& visited) {
        // Mark the current node as visited
        visited.insert(start);

        // Visit all the neighbors of the current node
        for (string neighbor : adjList[start]) {
            if (visited.find(neighbor) == visited.end()) {
                DFS(neighbor, visited);
            }
        }
    }

    // A method to perform a breadth-first search on the graph
    void BFS(string start, unordered_set<string>& visited) {
        // Create a queue to store the nodes to be visited
        queue<string> q;

        // Mark the current node as visited and enqueue it
        visited.insert(start);
        q.push(start);

        // While the queue is not empty
        while (!q.empty()) {
            // Dequeue the first node from the queue
            string u = q.front();
            q.pop();

            // Visit all the neighbors of the current node
            for (string neighbor : adjList[u]) {
                if (visited.find(neighbor) == visited.end()) {
                    visited.insert(neighbor);
                    q.push(neighbor);
                }
            }
        }
    }

    // A method to find the shortest path between two nodes in the graph
    vector<string> shortestPath(string start, string end) {
        // Create a map to store the parent of each node
        unordered_map<string, string> parent;

        // Create a queue to store the nodes to be visited
        queue<string> q;

        // Mark the current node as visited and enqueue it
        parent[start] = "";
        q.push(start);

        // While the queue is not empty
        while (!q.empty()) {
            // Dequeue the first node from the queue
            string u = q.front();
            q.pop();

            // If the current node is the destination, we have found the shortest path
            if (u == end) {
                // Reconstruct the shortest path by traversing the parent map
                vector<string> path;
                while (u != "") {
                    path.push_back(u);
                    u = parent[u];
                }

                // Reverse the path to get the correct order
                reverse(path.begin(), path.end());

                return path;
            }

            // Visit all the neighbors of the current node
            for (string neighbor : adjList[u]) {
                if (parent.find(neighbor) == parent.end()) {
                    parent[neighbor] = u;
                    q.push(neighbor);
                }
            }
        }

        // If we did not find a path, return an empty vector
        return vector<string>();
    }
};

// A function to print the adjacency list of a graph
void printGraph(Graph& graph) {
    for (auto it : graph.adjList) {
        cout << it.first << ": ";
        for (auto neighbor : it.second) {
            cout << neighbor << " ";
        }
        cout << endl;
    }
}

// A function to test the graph implementation
int main() {
    // Create a graph
    Graph graph;

    // Add some edges to the graph
    graph.addEdge("A", "B");
    graph.addEdge("A", "C");
    graph.addEdge("B", "D");
    graph.addEdge("C", "E");
    graph.addEdge("D", "F");
    graph.addEdge("E", "F");

    // Print the adjacency list of the graph
    cout << "Adjacency list of the graph:" << endl;
    printGraph(graph);

    // Perform a depth-first search on the graph
    unordered_set<string> visited;
    graph.DFS("A", visited);

    // Print the nodes visited by the depth-first search
    cout << "Nodes visited by the depth-first search:" << endl;
    for (auto it : visited) {
        cout << it << " ";
    }
    cout << endl;

    // Perform a breadth-first search on the graph
    visited.clear();
    graph.BFS("A", visited);

    // Print the nodes visited by the breadth-first search
    cout << "Nodes visited by the breadth-first search:" << endl;
    for (auto it : visited) {
        cout << it << " ";
    }
    cout << endl;

    // Find the shortest path between two nodes in the graph
    vector<string> shortestPath = graph.shortestPath("A", "F");

    // Print the shortest path
    cout << "Shortest path between A and F:" << endl;
    for (auto it : shortestPath) {
        cout << it << " ";
    }
    cout << endl;

    return 0;
}
```

Explanation:

This C++ code demonstrates various graph algorithms, including depth-first search (DFS), breadth-first search (BFS), and finding the shortest path between two nodes in a graph. It includes a custom Graph class to represent and manipulate the graph. The code also contains functions to print the adjacency list of the graph and test the graph implementation.

Here's a detailed explanation of the code:

1.	Graph Class:

The Graph class is defined to represent a graph. It has an adjacency list as an unordered map to store the edges of the graph. The class also includes methods to add edges to the graph, perform DFS and BFS on the graph, and find the shortest path between two nodes.

2.	addEdge Method:

The addEdge method is used to add a directed edge between two nodes in the graph. It takes two strings as parameters, representing the source and destination nodes, and adds the destination node to the adjacency list of the source node.

3.	DFS Method:

The DFS method performs a depth-first search on the graph starting from a given node. It takes a string (the start node) and an unordered set (to keep track of visited nodes) as parameters. The method recursively visits all the nodes reachable from the start node.

4.	BFS Method:

The BFS method performs a breadth-first search on the graph starting from a given node. It takes a string (the start node) and an unordered set (to keep track of visited nodes) as parameters. The method uses a queue to visit nodes level by level, starting from the start node.

5.	shortestPath Method:

The shortestPath method finds the shortest path between two nodes in the graph. It takes two strings (the start and end nodes) as parameters and returns a vector of strings representing the shortest path from the start node to the end node. The method uses a parent map to keep track of the parent of each node during the search and reconstructs the shortest path by traversing the parent map.

6.	printGraph Function:

The printGraph function is used to print the adjacency list of a graph. It takes a Graph object as a parameter and prints the adjacency list in a readable format.

7.	main Function:

The main function is the entry point of the program. It creates a Graph object, adds edges to it, and then calls the printGraph function to print the adjacency list. It also performs DFS, BFS, and finds the shortest path between two nodes in the graph and prints the results.

This code demonstrates a comprehensive implementation of various graph algorithms, allowing you to manipulate and analyze graphs and find the shortest path between two nodes in a graph.