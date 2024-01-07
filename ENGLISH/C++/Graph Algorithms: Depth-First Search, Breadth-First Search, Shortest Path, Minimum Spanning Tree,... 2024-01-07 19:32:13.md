```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cmath>

using namespace std;

// Define a custom data structure to represent a graph
struct Graph {
    map<int, vector<int>> adjacency_list;  // Adjacency list to represent the graph
    int num_vertices;                    // Number of vertices in the graph
};

// Function to add an edge to the graph
void add_edge(Graph &graph, int u, int v) {
    graph.adjacency_list[u].push_back(v);
    graph.adjacency_list[v].push_back(u);  // Undirected graph, so add edge in both directions
}

// Function to perform depth-first search (DFS) on the graph
void DFS(Graph &graph, int start_vertex, vector<bool> &visited) {
    // Mark the current vertex as visited
    visited[start_vertex] = true;
    cout << start_vertex << " ";  // Print the current vertex

    // Recursively visit all adjacent vertices
    for (int adjacent_vertex : graph.adjacency_list[start_vertex]) {
        if (!visited[adjacent_vertex]) {
            DFS(graph, adjacent_vertex, visited);
        }
    }
}

// Function to find connected components in the graph using DFS
vector<vector<int>> find_connected_components(Graph &graph) {
    vector<vector<int>> connected_components;  // Vector to store connected components
    vector<bool> visited(graph.num_vertices, false);  // Vector to keep track of visited vertices

    // Iterate over all vertices in the graph
    for (int i = 0; i < graph.num_vertices; i++) {
        if (!visited[i]) {
            vector<int> connected_component;  // Vector to store a single connected component
            DFS(graph, i, visited);            // Perform DFS starting from the current vertex
            connected_components.push_back(connected_component);  // Add the connected component to the list
        }
    }

    return connected_components;
}

// Function to perform breadth-first search (BFS) on the graph
void BFS(Graph &graph, int start_vertex, vector<bool> &visited) {
    // Create a queue to perform BFS
    queue<int> queue;

    // Mark the starting vertex as visited and enqueue it
    visited[start_vertex] = true;
    queue.push(start_vertex);

    // While the queue is not empty
    while (!queue.empty()) {
        // Dequeue the front element of the queue
        int current_vertex = queue.front();
        queue.pop();

        // Print the current vertex
        cout << current_vertex << " ";

        // Iterate over all adjacent vertices
        for (int adjacent_vertex : graph.adjacency_list[current_vertex]) {
            // If the adjacent vertex is not visited, mark it as visited and enqueue it
            if (!visited[adjacent_vertex]) {
                visited[adjacent_vertex] = true;
                queue.push(adjacent_vertex);
            }
        }
    }
}

// Function to find the shortest path between two vertices using BFS
vector<int> shortest_path_BFS(Graph &graph, int start_vertex, int end_vertex) {
    // Create a queue to perform BFS
    queue<int> queue;

    // Create a vector to store the path
    vector<int> path;

    // Create a map to store the parent of each vertex
    map<int, int> parent;

    // Mark the starting vertex as visited and enqueue it
    visited[start_vertex] = true;
    queue.push(start_vertex);
    parent[start_vertex] = -1;  // Parent of the starting vertex is -1

    // While the queue is not empty
    while (!queue.empty()) {
        // Dequeue the front element of the queue
        int current_vertex = queue.front();
        queue.pop();

        // If the current vertex is the end vertex, reconstruct the path and return it
        if (current_vertex == end_vertex) {
            int current_path = end_vertex;
            while (current_path != -1) {
                path.push_back(current_path);
                current_path = parent[current_path];
            }
            reverse(path.begin(), path.end());  // Reverse the path to get the correct order
            return path;
        }

        // Iterate over all adjacent vertices
        for (int adjacent_vertex : graph.adjacency_list[current_vertex]) {
            // If the adjacent vertex is not visited, mark it as visited, enqueue it, and set its parent
            if (!visited[adjacent_vertex]) {
                visited[adjacent_vertex] = true;
                queue.push(adjacent_vertex);
                parent[adjacent_vertex] = current_vertex;
            }
        }
    }

    // If the path is not found, return an empty vector
    return path;
}

// Function to find the minimum spanning tree of a graph using Kruskal's algorithm
vector<pair<int, int>> Kruskal_MST(Graph &graph) {
    // Create a priority queue to store edges
    priority_queue<pair<int, pair<int, int>>> pq;

    // Create a vector to store the edges in the minimum spanning tree
    vector<pair<int, int>> MST;

    // Create a set to store the vertices that have been included in the MST
    set<int> MST_vertices;

    // Iterate over all edges in the graph
    for (auto edge : graph.adjacency_list) {
        for (int adjacent_vertex : edge.second) {
            // If the edge is not already in the MST, add it to the priority queue
            if (MST_vertices.find(edge.first) == MST_vertices.end() || MST_vertices.find(adjacent_vertex) == MST_vertices.end()) {
                pq.push(make_pair(-edge.first, make_pair(edge.first, adjacent_vertex)));
            }
        }
    }

    // While the priority queue is not empty
    while (!pq.empty()) {
        // Get the edge with the highest weight (negative weight)
        pair<int, pair<int, int>> edge = pq.top();
        pq.pop();

        // If the vertices of the edge are not already in the MST, add the edge to the MST and mark the vertices as visited
        if (MST_vertices.find(edge.second.first) == MST_vertices.end() || MST_vertices.find(edge.second.second) == MST_vertices.end()) {
            MST.push_back(edge.second);
            MST_vertices.insert(edge.second.first);
            MST_vertices.insert(edge.second.second);
        }
    }

    // Return the edges in the minimum spanning tree
    return MST;
}

// Function to find the topological order of a directed graph using DFS
vector<int> topological_sort(Graph &graph) {
    // Create a stack to store the topological order
    stack<int> stack;

    // Create a vector to store the visited vertices
    vector<bool> visited(graph.num_vertices, false);

    // Iterate over all vertices in the graph
    for (int i = 0; i < graph.num_vertices; i++) {
        if (!visited[i]) {
            topological_sort_DFS(graph, i, visited, stack);
        }
    }

    // Reverse the stack to get the topological order
    vector<int> topological_order;
    while (!stack.empty()) {
        topological_order.push_back(stack.top());
        stack.pop();
    }

    return topological_order;
}

// Function to perform DFS for topological sort
void topological_sort_DFS(Graph &graph, int current_vertex, vector<bool> &visited, stack<int> &stack) {
    // Mark the current vertex as visited
    visited[current_vertex] = true;

    // Recursively visit all adjacent vertices
    for (int adjacent_vertex : graph.adjacency_list[current_vertex]) {
        if (!visited[adjacent_vertex]) {
            topological_sort_DFS(graph, adjacent_vertex, visited, stack);
        }
    }

    // After visiting all adjacent vertices, push the current vertex onto the stack
    stack.push(current_vertex);
}

// Function to find the shortest path between two vertices in a weighted graph using Dijkstra's algorithm
vector<int> Dijkstra(Graph &graph, int start_vertex, int end_vertex) {
    // Create a map to store the distances from the starting vertex to all other vertices
    map<int