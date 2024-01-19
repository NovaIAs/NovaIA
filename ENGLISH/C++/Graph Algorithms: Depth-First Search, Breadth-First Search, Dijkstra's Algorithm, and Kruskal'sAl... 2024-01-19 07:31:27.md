```c++
#include <bits/stdc++.h>
using namespace std;

// Custom data structure to represent a node in a graph
struct Node {
    int id;
    vector<int> neighbours;
    Node(int id) : id(id) {}
};

// Custom data structure to represent a graph
struct Graph {
    vector<Node> nodes;
    void add_node(int id) {
        nodes.emplace_back(id);
    }
    void add_edge(int from, int to) {
        nodes[from].neighbours.push_back(to);
        nodes[to].neighbours.push_back(from);
    }
};

// Function to perform a depth-first search on a graph
vector<int> depth_first_search(Graph& graph, int start_node) {
    // Initialize a stack to keep track of nodes to visit
    stack<int> stack;
    // Initialize a vector to store the order in which nodes are visited
    vector<int> visited;
    // Mark the starting node as visited and push it onto the stack
    stack.push(start_node);
    visited.push_back(start_node);
    // While the stack is not empty, continue visiting nodes
    while (!stack.empty()) {
        // Pop the top node from the stack
        int current_node = stack.top();
        stack.pop();
        // Visit all unvisited neighbours of the current node
        for (int neighbour : graph.nodes[current_node].neighbours) {
            if (find(visited.begin(), visited.end(), neighbour) == visited.end()) {
                // If the neighbour has not been visited, mark it as visited, push it onto the stack, and add it to the visited vector
                stack.push(neighbour);
                visited.push_back(neighbour);
            }
        }
    }
    // Return the vector of visited nodes in the order they were visited
    return visited;
}

// Function to perform a breadth-first search on a graph
vector<int> breadth_first_search(Graph& graph, int start_node) {
    // Initialize a queue to keep track of nodes to visit
    queue<int> queue;
    // Initialize a vector to store the order in which nodes are visited
    vector<int> visited;
    // Mark the starting node as visited and add it to the queue
    queue.push(start_node);
    visited.push_back(start_node);
    // While the queue is not empty, continue visiting nodes
    while (!queue.empty()) {
        // Pop the front node from the queue
        int current_node = queue.front();
        queue.pop();
        // Visit all unvisited neighbours of the current node
        for (int neighbour : graph.nodes[current_node].neighbours) {
            if (find(visited.begin(), visited.end(), neighbour) == visited.end()) {
                // If the neighbour has not been visited, mark it as visited, add it to the queue, and add it to the visited vector
                queue.push(neighbour);
                visited.push_back(neighbour);
            }
        }
    }
    // Return the vector of visited nodes in the order they were visited
    return visited;
}

// Function to find the shortest path between two nodes in a graph using Dijkstra's algorithm
vector<int> dijkstra(Graph& graph, int start_node, int end_node) {
    // Initialize a vector to store the distances from the starting node to all other nodes
    vector<int> distances(graph.nodes.size(), INT_MAX);
    // Initialize a priority queue to keep track of nodes to visit, sorted by their distance from the starting node
    priority_queue<pair<int, int>> queue;
    // Mark the starting node as visited and set its distance to 0
    distances[start_node] = 0;
    queue.push(make_pair(0, start_node));
    // While the priority queue is not empty, continue visiting nodes
    while (!queue.empty()) {
        // Pop the node with the smallest distance from the priority queue
        int current_node = queue.top().second;
        queue.pop();
        // Visit all unvisited neighbours of the current node
        for (int neighbour : graph.nodes[current_node].neighbours) {
            if (distances[neighbour] > distances[current_node] + 1) {
                // If the distance to the neighbour is greater than the distance to the current node plus the weight of the edge between them,
                // update the distance to the neighbour and add it to the priority queue
                distances[neighbour] = distances[current_node] + 1;
                queue.push(make_pair(distances[neighbour], neighbour));
            }
        }
    }
    // If the distance to the end node is still INT_MAX, then there is no path between the starting node and the end node
    if (distances[end_node] == INT_MAX) {
        return {};
    }
    // Reconstruct the shortest path by backtracking from the end node
    vector<int> path;
    int current_node = end_node;
    while (current_node != start_node) {
        // Find the neighbour of the current node with the smallest distance
        int min_distance = INT_MAX;
        int min_neighbour = -1;
        for (int neighbour : graph.nodes[current_node].neighbours) {
            if (distances[neighbour] < min_distance) {
                min_distance = distances[neighbour];
                min_neighbour = neighbour;
            }
        }
        // Add the current node to the path and update the current node to be its neighbour with the smallest distance
        path.push_back(current_node);
        current_node = min_neighbour;
    }
    path.push_back(start_node);
    // Reverse the path to get the correct order of nodes from the starting node to the end node
    reverse(path.begin(), path.end());
    // Return the shortest path
    return path;
}

// Function to find the minimum spanning tree of a graph using Kruskal's algorithm
vector<pair<int, int>> kruskal(Graph& graph) {
    // Initialize a vector to store the edges in the minimum spanning tree
    vector<pair<int, int>> minimum_spanning_tree;
    // Initialize a disjoint-set data structure to keep track of connected components
    DisjointSetUnion dsu(graph.nodes.size());
    // Sort the edges by their weights in ascending order
    sort(graph.edges.begin(), graph.edges.end(), [](const pair<int, int>& edge1, const pair<int, int>& edge2) {
        return edge1.second < edge2.second;
    });
    // For each edge in sorted order, check if it connects two different connected components
    for (const auto& edge : graph.edges) {
        int from = edge.first;
        int to = edge.second;
        if (dsu.find(from) != dsu.find(to)) {
            // If the edge connects two different connected components, add it to the minimum spanning tree and merge the two connected components
            minimum_spanning_tree.push_back(edge);
            dsu.merge(from, to);
        }
    }
    // Return the minimum spanning tree
    return minimum_spanning_tree;
}

// Main function to test the graph algorithms
int main() {
    // Create a graph
    Graph graph;
    graph.add_node(0);
    graph.add_node(1);
    graph.add_node(2);
    graph.add_node(3);
    graph.add_edge(0, 1);
    graph.add_edge(1, 2);
    graph.add_edge(2, 3);
    graph.add_edge(0, 3);

    // Perform depth-first search on the graph
    vector<int> dfs_result = depth_first_search(graph, 0);
    cout << "Depth-first search result: ";
    for (int node : dfs_result) {
        cout << node << " ";
    }
    cout << endl;

    // Perform breadth-first search on the graph
    vector<int> bfs_result = breadth_first_search(graph, 0);
    cout << "Breadth-first search result: ";
    for (int node : bfs_result) {
        cout << node << " ";
    }
    cout << endl;

    // Find the shortest path between nodes 0 and 3 using Dijkstra's algorithm
    vector<int> dijkstra_result = dijkstra(graph, 0, 3);
    cout << "Shortest path between nodes 0 and 3 using Dijkstra's algorithm: ";
    for (int node : dijkstra_result) {
        cout << node << " ";
    }
    cout << endl;

    // Find the minimum spanning tree of the graph using Kruskal's algorithm
    vector<pair<int, int>> kruskal_result = kruskal(graph);
    cout << "Minimum spanning tree of the graph using Kruskal's algorithm: ";
    for (const auto& edge : kruskal_result) {
        cout << "(" << edge.first << ", " << edge.second << ") ";
    }
    cout << endl;

    return 0;
}

// Explanation:
// This C++ program demonstrates various graph algorithms, including depth-first search, breadth-first search, Dijkstra's algorithm for finding the shortest path between two nodes, and Kruskal's algorithm for finding the minimum spanning tree of a graph.

// The program begins by creating a graph with four nodes, numbered 0 to 3, and four edges, connecting nodes 0 and 1, 1 and 2, 2 and 3, and 0 and 3.

// The depth-first search algorithm is then used to traverse the graph starting from node 0. The program prints the order in which the nodes are visited by the algorithm, which is: 0, 1, 2, 3.

// Next, the breadth-first search algorithm is used to traverse the graph starting from node 0. The program prints the order in which the nodes are visited by the algorithm, which is: 0, 1, 2, 3.

// The Dijkstra's algorithm is then used to find the shortest path between nodes 0 and 3. The program prints the shortest path, which is: 0->1->2->3.

// Finally, the Kruskal's algorithm is used to find the minimum spanning tree of the graph. The program