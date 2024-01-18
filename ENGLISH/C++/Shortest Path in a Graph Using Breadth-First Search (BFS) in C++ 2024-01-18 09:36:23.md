```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>

using namespace std;

// Define a custom data structure to represent a graph
struct Graph {
    // Adjacency list to represent the graph
    map<int, vector<int>> adjList;
    // Map to store the distance of each node from the source node
    map<int, int> distance;
    // Set to keep track of visited nodes
    set<int> visited;

    // Function to add an edge between two nodes
    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        adjList[v].push_back(u);
    }

    // Function to perform breadth-first search (BFS) from a given source node
    void BFS(int source) {
        // Initialize the distance of the source node to 0
        distance[source] = 0;
        // Create a queue to store the nodes to be visited
        queue<int> q;
        // Enqueue the source node
        q.push(source);
        // Mark the source node as visited
        visited.insert(source);

        // While the queue is not empty
        while (!q.empty()) {
            // Dequeue the front node of the queue
            int u = q.front();
            q.pop();

            // Visit all adjacent nodes of the dequeued node
            for (int v : adjList[u]) {
                // If the adjacent node has not been visited
                if (visited.find(v) == visited.end()) {
                    // Set the distance of the adjacent node to the distance of the current node plus 1
                    distance[v] = distance[u] + 1;
                    // Enqueue the adjacent node
                    q.push(v);
                    // Mark the adjacent node as visited
                    visited.insert(v);
                }
            }
        }
    }

    // Function to find the shortest path between two nodes
    int shortestPath(int source, int destination) {
        // Perform BFS from the source node
        BFS(source);

        // If the destination node is not reachable from the source node
        if (distance.find(destination) == distance.end()) {
            return -1;
        }

        // Return the distance of the destination node from the source node
        return distance[destination];
    }
};

// Main function
int main() {
    // Create a graph
    Graph graph;

    // Add edges to the graph
    graph.addEdge(0, 1);
    graph.addEdge(0, 2);
    graph.addEdge(1, 2);
    graph.addEdge(1, 3);
    graph.addEdge(2, 4);
    graph.addEdge(3, 4);

    // Find the shortest path between nodes 0 and 4
    int shortestPath = graph.shortestPath(0, 4);

    // Print the shortest path
    cout << "The shortest path between nodes 0 and 4 is: " << shortestPath << endl;

    return 0;
}
```

This code implements a graph data structure in C++ and uses it to find the shortest path between two nodes in the graph using breadth-first search (BFS).

The code first defines a `Graph` struct, which represents the graph. The `Graph` struct contains an adjacency list to represent the graph, a map to store the distance of each node from the source node, and a set to keep track of visited nodes.

The code then defines a function `addEdge` to add an edge between two nodes in the graph.

The code then defines a function `BFS` to perform BFS from a given source node. The function takes the source node as an argument and uses a queue to store the nodes to be visited. The function marks each visited node and updates the distance of each node from the source node.

The code then defines a function `shortestPath` to find the shortest path between two nodes in the graph. The function takes the source and destination nodes as arguments and calls the `BFS` function to calculate the distance of each node from the source node. The function then returns the distance of the destination node from the source node, or -1 if the destination node is not reachable from the source node.

Finally, the code creates a graph, adds edges to the graph, and then calls the `shortestPath` function to find the shortest path between two nodes in the graph. The code then prints the shortest path.