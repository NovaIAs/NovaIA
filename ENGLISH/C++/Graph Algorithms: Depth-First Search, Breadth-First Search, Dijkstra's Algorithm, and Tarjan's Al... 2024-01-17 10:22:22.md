```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// Define a custom data structure to represent a node in a graph
struct Node {
    int id;
    vector<Node*> neighbors;

    Node(int id) : id(id) {}
};

// Define a class to represent a graph
class Graph {
public:
    // Constructor
    Graph() {}

    // Add a node to the graph
    void addNode(Node* node) {
        nodes.push_back(node);
    }

    // Add an edge between two nodes
    void addEdge(Node* node1, Node* node2) {
        node1->neighbors.push_back(node2);
        node2->neighbors.push_back(node1);
    }

    // Perform a depth-first search on the graph starting from a given node
    void dfs(Node* start) {
        // Create a stack to store the nodes to be visited
        stack<Node*> stack;

        // Mark the start node as visited and push it onto the stack
        start->visited = true;
        stack.push(start);

        // While the stack is not empty, pop the top node and visit its neighbors
        while (!stack.empty()) {
            Node* current = stack.top();
            stack.pop();

            // Visit the current node
            cout << "Visiting node " << current->id << endl;

            // For each neighbor of the current node, if it has not been visited yet,
            // mark it as visited and push it onto the stack
            for (Node* neighbor : current->neighbors) {
                if (!neighbor->visited) {
                    neighbor->visited = true;
                    stack.push(neighbor);
                }
            }
        }
    }

    // Perform a breadth-first search on the graph starting from a given node
    void bfs(Node* start) {
        // Create a queue to store the nodes to be visited
        queue<Node*> queue;

        // Mark the start node as visited and enqueue it
        start->visited = true;
        queue.push(start);

        // While the queue is not empty, dequeue the front node and visit its neighbors
        while (!queue.empty()) {
            Node* current = queue.front();
            queue.pop();

            // Visit the current node
            cout << "Visiting node " << current->id << endl;

            // For each neighbor of the current node, if it has not been visited yet,
            // mark it as visited and enqueue it
            for (Node* neighbor : current->neighbors) {
                if (!neighbor->visited) {
                    neighbor->visited = true;
                    queue.push(neighbor);
                }
            }
        }
    }

    // Find the shortest path between two nodes in the graph using Dijkstra's algorithm
    vector<Node*> dijkstra(Node* start, Node* end) {
        // Initialize distances to all nodes as infinity
        map<Node*, int> distances;
        for (Node* node : nodes) {
            distances[node] = INT_MAX;
        }

        // Set the distance to the start node to 0
        distances[start] = 0;

        // Create a priority queue to store the nodes to be visited, sorted by their distances
        priority_queue<pair<int, Node*>, vector<pair<int, Node*>>, greater<pair<int, Node*>>> pq;
        pq.push(make_pair(0, start));

        // While the priority queue is not empty, pop the top node and visit its neighbors
        while (!pq.empty()) {
            pair<int, Node*> current = pq.top();
            pq.pop();

            // If the current node is the end node, we have found the shortest path
            if (current.second == end) {
                break;
            }

            // For each neighbor of the current node, if it has not been visited yet or if the
            // distance to the neighbor is greater than the distance to the current node plus the
            // weight of the edge between the current node and the neighbor, update the distance
            // to the neighbor and push it onto the priority queue
            for (Node* neighbor : current.second->neighbors) {
                int weight = 1; // Assuming all edges have a weight of 1
                if (distances[neighbor] > distances[current.second] + weight) {
                    distances[neighbor] = distances[current.second] + weight;
                    pq.push(make_pair(distances[neighbor], neighbor));
                }
            }
        }

        // If the distance to the end node is still infinity, there is no path between the start
        // and end nodes
        if (distances[end] == INT_MAX) {
            return vector<Node*>();
        }

        // Reconstruct the shortest path by backtracking from the end node
        vector<Node*> path;
        Node* current = end;
        while (current != start) {
            path.push_back(current);
            for (Node* neighbor : current->neighbors) {
                if (distances[neighbor] + 1 == distances[current]) {
                    current = neighbor;
                    break;
                }
            }
        }
        path.push_back(start);

        // Reverse the path to get the correct order
        reverse(path.begin(), path.end());

        return path;
    }

    // Find all the cycles in the graph using Tarjan's algorithm
    vector<vector<Node*>> tarjan() {
        // Initialize the stack and the visited set
        stack<Node*> stack;
        set<Node*> visited;

        // Initialize the low and num values for each node
        map<Node*, int> low;
        map<Node*, int> num;
        for (Node* node : nodes) {
            low[node] = -1;
            num[node] = -1;
        }

        // Initialize the cycles vector
        vector<vector<Node*>> cycles;

        // Perform a depth-first search on the graph
        int time = 0;
        for (Node* node : nodes) {
            if (num[node] == -1) {
                dfsTarjan(node, stack, visited, low, num, time, cycles);
            }
        }

        return cycles;
    }

private:
    // Perform a depth-first search on the graph for Tarjan's algorithm
    void dfsTarjan(Node* current, stack<Node*>& stack, set<Node*>& visited, map<Node*, int>& low, map<Node*, int>& num, int& time, vector<vector<Node*>>& cycles) {
        // Mark the current node as visited and push it onto the stack
        visited.insert(current);
        stack.push(current);

        // Set the low and num values for the current node
        low[current] = num[current] = time++;