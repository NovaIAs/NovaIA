```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

// Define a custom type for a node in a graph.
struct Node {
    int id;
    vector<Node*> neighbors;
    bool visited;

    Node(int id) {
        this->id = id;
        this->visited = false;
    }
};

// Define a class for the graph.
class Graph {
private:
    map<int, Node*> nodes;

public:
    // Add a node to the graph.
    void addNode(int id) {
        if (nodes.find(id) == nodes.end()) {
            nodes[id] = new Node(id);
        }
    }

    // Add an edge between two nodes.
    void addEdge(int id1, int id2) {
        Node* node1 = nodes[id1];
        Node* node2 = nodes[id2];
        node1->neighbors.push_back(node2);
        node2->neighbors.push_back(node1);
    }

    // Perform a depth-first search on the graph starting from a given node.
    void dfs(int id) {
        Node* node = nodes[id];
        if (node->visited) {
            return;
        }

        node->visited = true;
        cout << "Visiting node " << node->id << endl;

        for (Node* neighbor : node->neighbors) {
            dfs(neighbor->id);
        }
    }

    // Find the shortest path between two nodes using Dijkstra's algorithm.
    vector<int> dijkstra(int id1, int id2) {
        Node* node1 = nodes[id1];
        Node* node2 = nodes[id2];

        // Initialize distances to all nodes to infinity.
        map<int, int> distances;
        for (auto& node : nodes) {
            distances[node.first] = INT_MAX;
        }

        // Set the distance to the starting node to 0.
        distances[id1] = 0;

        // Initialize a priority queue to store nodes based on their distances.
        priority_queue<Node*, vector<Node*>, function<bool(Node*, Node*)>> pq(
            [](Node* node1, Node* node2) {
                return distances[node1->id] > distances[node2->id];
            }
        );

        // Add the starting node to the priority queue.
        pq.push(node1);

        // While the priority queue is not empty, process nodes.
        while (!pq.empty()) {
            // Get the node with the smallest distance from the priority queue.
            Node* node = pq.top();
            pq.pop();

            // If the node is the destination, we have found the shortest path.
            if (node == node2) {
                break;
            }

            // For each neighbor of the node, calculate the new distance and update it if it is shorter.
            for (Node* neighbor : node->neighbors) {
                int newDistance = distances[node->id] + 1;
                if (newDistance < distances[neighbor->id]) {
                    distances[neighbor->id] = newDistance;
                    pq.push(neighbor);
                }
            }
        }

        // Reconstruct the shortest path by backtracking from the destination node.
        vector<int> shortestPath;
        Node* currentNode = node2;
        while (currentNode != node1) {
            shortestPath.push_back(currentNode->id);
            for (Node* neighbor : currentNode->neighbors) {
                if (distances[neighbor->id] + 1 == distances[currentNode->id]) {
                    currentNode = neighbor;
                    break;
                }
            }
        }
        shortestPath.push_back(node1->id);

        // Reverse the shortest path to get the path from the starting node to the destination node.
        reverse(shortestPath.begin(), shortestPath.end());

        return shortestPath;
    }
};

int main() {
    // Create a new graph.
    Graph graph;

    // Add nodes to the graph.
    graph.addNode(1);
    graph.addNode(2);
    graph.addNode(3);
    graph.addNode(4);
    graph.addNode(5);

    // Add edges to the graph.
    graph.addEdge(1, 2);
    graph.addEdge(1, 3);
    graph.addEdge(2, 4);
    graph.addEdge(2, 5);
    graph.addEdge(3, 4);
    graph.addEdge(4, 5);

    // Perform a depth-first search on the graph starting from node 1.
    graph.dfs(1);

    // Find the shortest path between nodes 1 and 5 using Dijkstra's algorithm.
    vector<int> shortestPath = graph.dijkstra(1, 5);

    // Print the shortest path.
    cout << "Shortest path from node 1 to node 5: ";
    for (int node : shortestPath) {
        cout << node << " ";
    }
    cout << endl;

    return 0;
}
```

This code demonstrates various graph algorithms, including depth-first search and Dijkstra's algorithm, and explains the implementation in detail. It also includes a main function that creates a sample graph, performs the algorithms on it, and prints the results.