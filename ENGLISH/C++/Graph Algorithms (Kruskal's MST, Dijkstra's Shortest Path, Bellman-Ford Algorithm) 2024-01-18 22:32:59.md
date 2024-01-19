```c++
#include <iostream>
#include <vector>
#include <unordered_map>
#include <queue>
#include <algorithm>

using namespace std;

// Data structure to represent a graph edge
struct Edge {
    int src, dest, weight;
};

// Function to compare two edges based on their weights
bool compareEdges(const Edge& edge1, const Edge& edge2) {
    return edge1.weight < edge2.weight;
}

// Function to find the root of a subset in a disjoint set union data structure
int find(vector<int>& parent, int v) {
    if (parent[v] == v) {
        return v;
    }
    return parent[v] = find(parent, parent[v]);
}

// Function to perform union of two subsets in a disjoint set union data structure
void union(vector<int>& parent, vector<int>& rank, int v1, int v2) {
    int root1 = find(parent, v1);
    int root2 = find(parent, v2);

    if (root1 == root2) {
        return;
    }

    if (rank[root1] < rank[root2]) {
        parent[root1] = root2;
    } else if (rank[root1] > rank[root2]) {
        parent[root2] = root1;
    } else {
        parent[root2] = root1;
        rank[root1]++;
    }
}

// Function to perform Kruskal's algorithm to find the minimum spanning tree of a graph
vector<Edge> KruskalMST(vector<Edge>& edges, int numVertices) {
    // Sort the edges in ascending order of their weights
    sort(edges.begin(), edges.end(), compareEdges);

    // Initialize the disjoint set union data structure
    vector<int> parent(numVertices);
    vector<int> rank(numVertices, 0);
    for (int i = 0; i < numVertices; i++) {
        parent[i] = i;
    }

    // Initialize the minimum spanning tree
    vector<Edge> mst;

    // Iterate over the sorted edges
    for (const Edge& edge : edges) {
        // Find the root of the subset containing the source vertex
        int root1 = find(parent, edge.src);
        // Find the root of the subset containing the destination vertex
        int root2 = find(parent, edge.dest);

        // If the source and destination vertices are in different subsets, add the edge to the MST and perform union
        if (root1 != root2) {
            mst.push_back(edge);
            union(parent, rank, root1, root2);
        }
    }

    return mst;
}

// Function to find the shortest path between two vertices in a weighted graph using Dijkstra's algorithm
vector<int> Dijkstra(const vector<vector<pair<int, int>>>& graph, int source, int destination) {
    // Initialize the distance of all vertices from the source to infinity, except for the source vertex which is set to 0
    vector<int> distance(graph.size(), numeric_limits<int>::max());
    distance[source] = 0;

    // Initialize the priority queue to store the vertices to be visited, sorted by their distance from the source
    priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> pq;
    pq.push(make_pair(0, source));

    // Initialize the visited array to keep track of which vertices have been visited
    vector<bool> visited(graph.size(), false);

    // While there are still vertices to be visited
    while (!pq.empty()) {
        // Get the vertex with the smallest distance from the source
        int u = pq.top().second;
        pq.pop();

        // If the current vertex is the destination, we have found the shortest path
        if (u == destination) {
            break;
        }

        // Mark the current vertex as visited
        visited[u] = true;

        // Iterate over the neighbors of the current vertex
        for (const auto& edge : graph[u]) {
            int v = edge.first;
            int weight = edge.second;

            // If the neighbor has not been visited and the distance to the neighbor is less than the current distance, update the distance and push the neighbor to the priority queue
            if (!visited[v] && distance[u] + weight < distance[v]) {
                distance[v] = distance[u] + weight;
                pq.push(make_pair(distance[v], v));
            }
        }
    }

    // If the distance to the destination is still infinity, there is no path between the source and the destination
    if (distance[destination] == numeric_limits<int>::max()) {
        return {};
    }

    // Reconstruct the shortest path
    vector<int> path;
    int current = destination;
    while (current != source) {
        path.push_back(current);
        for (const auto& edge : graph[current]) {
            int v = edge.first;
            int weight = edge.second;
            if (distance[current] - weight == distance[v]) {
                current = v;
                break;
            }
        }
    }
    path.push_back(source);
    reverse(path.begin(), path.end());

    return path;
}

// Function to find the minimum cost path between two vertices in a directed graph using Bellman-Ford algorithm
vector<int> BellmanFord(const vector<vector<pair<int, int>>>& graph, int source, int destination) {
    // Initialize the distance of all vertices from the source to infinity, except for the source vertex which is set to 0
    vector<int> distance(graph.size(), numeric_limits<int>::max());
    distance[source] = 0;

    // Iterate over all vertices
    for (int i = 0; i < graph.size() - 1; i++) {
        // Iterate over all edges
        for (int u = 0; u < graph.size(); u++) {
            for (const auto& edge : graph[u]) {
                int v = edge.first;
                int weight = edge.second;

                // If the distance to the neighbor is less than the current distance, update the distance
                if (distance[u] + weight < distance[v]) {
                    distance[v] = distance[u] + weight;
                }
            }
        }
    }

    // Check for negative cycles
    for (int u = 0; u < graph.size(); u++) {
        for (const auto& edge : graph[u]) {
            int v = edge.first;
            int weight = edge.second;

            if (distance[u] + weight < distance[v]) {
                // Negative cycle detected
                return {};
            }
        }
    }

    // If the distance to the destination is still infinity, there is no path between the source and the destination
    if (distance[destination] == numeric_limits<int>::max()) {
        return {};
    }

    // Reconstruct the shortest path
    vector<int> path;
    int current = destination;
    while (current != source) {
        path.push_back(current);
        for (const auto& edge : graph[current]) {
            int v = edge.first;
            int weight = edge.second;
            if (distance[current] - weight == distance[v