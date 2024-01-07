```c++
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>

using namespace std;

// Define a custom data structure to represent a node in a graph
struct Node {
    int id;
    vector<Node*> neighbors;

    Node(int id) : id(id) {}
};

// Define a custom data structure to represent a graph
struct Graph {
    vector<Node*> nodes;

    // Add a node to the graph
    void addNode(int id) {
        nodes.push_back(new Node(id));
    }

    // Add an edge between two nodes in the graph
    void addEdge(int id1, int id2) {
        Node* node1 = findNode(id1);
        Node* node2 = findNode(id2);
        node1->neighbors.push_back(node2);
        node2->neighbors.push_back(node1);
    }

    // Find a node in the graph by its ID
    Node* findNode(int id) {
        for (Node* node : nodes) {
            if (node->id == id) {
                return node;
            }
        }
        return nullptr;
    }

    // Perform a depth-first search on the graph starting from a given node
    void DFS(Node* start) {
        stack<Node*> stack;
        set<Node*> visited;

        stack.push(start);
        visited.insert(start);

        while (!stack.empty()) {
            Node* current = stack.top();
            stack.pop();

            cout << "Visiting node " << current->id << endl;

            for (Node* neighbor : current->neighbors) {
                if (visited.find(neighbor) == visited.end()) {
                    stack.push(neighbor);
                    visited.insert(neighbor);
                }
            }
        }
    }

    // Perform a breadth-first search on the graph starting from a given node
    void BFS(Node* start) {
        queue<Node*> queue;
        set<Node*> visited;

        queue.push(start);
        visited.insert(start);

        while (!queue.empty()) {
            Node* current = queue.front();
            queue.pop();

            cout << "Visiting node " << current->id << endl;

            for (Node* neighbor : current->neighbors) {
                if (visited.find(neighbor) == visited.end()) {
                    queue.push(neighbor);
                    visited.insert(neighbor);
                }
            }
        }
    }

    // Find the shortest path between two nodes in the graph using Dijkstra's algorithm
    vector<Node*> Dijkstra(Node* start, Node* end) {
        map<Node*, int> distances;
        map<Node*, Node*> previous;
        set<Node*> visited;

        for (Node* node : nodes) {
            distances[node] = INT_MAX;
            previous[node] = nullptr;
        }

        distances[start] = 0;

        while (!visited.empty()) {
            Node* current = nullptr;
            int minDistance = INT_MAX;

            for (Node* node : nodes) {
                if (visited.find(node) == visited.end() && distances[node] < minDistance) {
                    minDistance = distances[node];
                    current = node;
                }
            }

            if (current == nullptr) {
                break;
            }

            visited.insert(current);

            for (Node* neighbor : current->neighbors) {
                if (visited.find(neighbor) == visited.end()) {
                    int newDistance = distances[current] + 1;
                    if (newDistance < distances[neighbor]) {
                        distances[neighbor] = newDistance;
                        previous[neighbor] = current;
                    }
                }
            }
        }

        vector<Node*> path;
        Node* current = end;

        while (current != nullptr) {
            path.push_back(current);
            current = previous[current];
        }

        reverse(path.begin(), path.end());

        return path;
    }
};

int main() {
    // Create a graph
    Graph graph;
    graph.addNode(1);
    graph.addNode(2);
    graph.addNode(3);
    graph.addNode(4);
    graph.addNode(5);
    graph.addEdge(1, 2);
    graph.addEdge(1, 3);
    graph.addEdge(2, 4);
    graph.addEdge(2, 5);
    graph.addEdge(3, 5);

    // Perform a DFS on the graph starting from node 1
    cout << "DFS traversal:" << endl;
    graph.DFS(graph.findNode(1));

    // Perform a BFS on the graph starting from node 1
    cout << "BFS traversal:" << endl;
    graph.BFS(graph.findNode(1));

    // Find the shortest path between nodes 1 and 5 using Dijkstra's algorithm
    cout << "Shortest path between nodes 1 and 5:" << endl;
    vector<Node*> path = graph.Dijkstra(graph.findNode(1), graph.findNode(5));
    for (Node* node : path) {
        cout << node->id << " ";
    }
    cout << endl;

    return 0;
}
```
This code implements a graph data structure and various graph algorithms in C++. It includes a custom data structure for representing nodes and edges in a graph, as well as methods for adding nodes and edges to the graph. The code also includes implementations of depth-first search (DFS), breadth-first search (BFS), and Dijkstra's algorithm for finding the shortest path between two nodes in a graph. The code is well-commented and organized, making it easy to understand and modify.