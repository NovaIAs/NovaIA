```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cmath>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <string>

using namespace std;

// Define a macro for easier input/output
#define io ios_base::sync_with_stdio(false); cin.tie(NULL); cout.tie(NULL);

// Define a macro for printing vectors
#define print_vector(v) \
    for (auto& i : v) { cout << i << " "; } cout << endl;

// Define a macro for printing maps
#define print_map(m) \
    for (auto& [key, val] : m) { cout << key << " => " << val << endl; }

// Define a macro for printing sets
#define print_set(s) \
    for (auto& i : s) { cout << i << " "; } cout << endl;

// Define a macro for printing priority queues
#define print_priority_queue(pq) \
    while (!pq.empty()) { cout << pq.top() << " "; pq.pop(); } cout << endl;

// Define a macro for printing stacks
#define print_stack(s) \
    while (!s.empty()) { cout << s.top() << " "; s.pop(); } cout << endl;

// Define a macro for printing binary trees
#define print_binary_tree(root) \
    _print_binary_tree(root, 0);

// Helper function for printing binary trees
void _print_binary_tree(TreeNode* root, int level) {
    if (root == nullptr) {
        return;
    }
    for (int i = 0; i < level; i++) {
        cout << "  ";
    }
    cout << root->val << endl;
    _print_binary_tree(root->left, level + 1);
    _print_binary_tree(root->right, level + 1);
}

// Define a class for representing a node in a graph
class Node {
public:
    int val;
    vector<Node*> neighbors;

    Node(int val) : val(val) {}
};

// Define a class for representing a graph
class Graph {
public:
    vector<Node*> nodes;

    void add_node(Node* node) {
        nodes.push_back(node);
    }

    void add_edge(Node* from, Node* to) {
        from->neighbors.push_back(to);
    }

    void print() {
        for (auto& node : nodes) {
            cout << node->val << " => ";
            for (auto& neighbor : node->neighbors) {
                cout << neighbor->val << " ";
            }
            cout << endl;
        }
    }
};

// Define a function for finding the shortest path between two nodes in a graph
vector<Node*> shortest_path(Graph& graph, Node* from, Node* to) {
    // Initialize a queue for performing BFS
    queue<Node*> q;

    // Initialize a map to store the parent of each node
    map<Node*, Node*> parent;

    // Add the starting node to the queue and mark it as visited
    q.push(from);
    parent[from] = nullptr;

    // While the queue is not empty
    while (!q.empty()) {
        // Dequeue the first node from the queue
        Node* current = q.front();
        q.pop();

        // If the current node is the destination, we have found the shortest path
        if (current == to) {
            // Reconstruct the shortest path by following the parent pointers
            vector<Node*> path;
            while (current != nullptr) {
                path.push_back(current);
                current = parent[current];
            }
            reverse(path.begin(), path.end());
            return path;
        }

        // Visit all the neighbors of the current node
        for (auto& neighbor : current->neighbors) {
            // If the neighbor has not been visited yet, add it to the queue and mark it as visited
            if (parent.find(neighbor) == parent.end()) {
                q.push(neighbor);
                parent[neighbor] = current;
            }
        }
    }

    // If we reach here, there is no path between the two nodes
    return {};
}

// Define a function for finding the minimum spanning tree of a graph
vector<pair<Node*, Node*>> minimum_spanning_tree(Graph& graph) {
    // Initialize a priority queue for storing the edges in ascending order of weight
    priority_queue<pair<int, pair<Node*, Node*>>, vector<pair<int, pair<Node*, Node*>>>, greater<pair<int, pair<Node*, Node*>>>> pq;

    // Initialize a set to store the nodes that have been visited
    set<Node*> visited;

    // Initialize a vector to store the edges in the minimum spanning tree
    vector<pair<Node*, Node*>> mst;

    // Add all the edges to the priority queue
    for (auto& node : graph.nodes) {
        for (auto& neighbor : node->neighbors) {
            if (visited.find(neighbor) == visited.end()) {
                pq.push(make_pair(node->val + neighbor->val, make_pair(node, neighbor)));
            }
        }
    }

    // While the priority queue is not empty
    while (!pq.empty()) {
        // Dequeue the edge with the minimum weight
        pair<int, pair<Node*, Node*>> current_edge = pq.top();
        pq.pop();

        // If the nodes at the ends of the edge have not been visited yet, add the edge to the minimum spanning tree and mark the nodes as visited
        if (visited.find(current_edge.second.first) == visited.end() && visited.find(current_edge.second.second) == visited.end()) {
            mst.push_back(current_edge.second);
            visited.insert(current_edge.second.first);
            visited.insert(current_edge.second.second);

            // Add all the edges connected to the newly visited nodes to the priority queue
            for (auto& neighbor : current_edge.second.first->neighbors) {
                if (visited.find(neighbor) == visited.end()) {
                    pq.push(make_pair(current_edge.second.first->val + neighbor->val, make_pair(current_edge.second.first, neighbor)));
                }
            }
            for (auto& neighbor : current_edge.second.second->neighbors) {
                if (visited.find(neighbor) == visited.end()) {
                    pq.push(make_pair(current_edge.second.second->val + neighbor->val, make_pair(current_edge.second.second, neighbor)));
                }
            }
        }
    }

    // Return the minimum spanning tree
    return mst;
}

// Define a function for finding the longest path in a directed acyclic graph
vector<Node*> longest_path(Graph& graph) {
    // Topologically sort the graph
    vector<Node*> topological_order = topological_sort(graph);

    // Initialize a map to store the longest path from each node to the starting node
    map<Node*, vector<Node*>> longest_paths;

    // Initialize the longest path from the starting node to itself to be an empty vector
    longest_paths[topological_order[0]] = {};

    // For each node in the topological order, find the longest path from each of its neighbors to the starting node
    for (int i = 1; i < topological_order.size(); i++) {
        Node* current = topological_order[i];
        for (auto& neighbor : current->neighbors) {
            // If the longest path from the neighbor to the starting node is longer than the current longest path from the current node to the starting node, update the current longest path
            if (longest_paths[neighbor].size() + 1 > longest_paths[current].size()) {
                longest_paths[current] = longest_paths[neighbor];
                longest_paths[current].push_back(current);
            }
        }
    }

    // Find the node with the longest path to the starting node
    Node* longest_path_node = nullptr;
    int longest_path_length = 0;
    for (auto& [node, path] : longest_paths) {
        if (path.size() > longest_path_length) {
            longest_path_node = node;
            longest_path_length = path.size();
        }
    }

    // Return the longest path
    return longest_paths[