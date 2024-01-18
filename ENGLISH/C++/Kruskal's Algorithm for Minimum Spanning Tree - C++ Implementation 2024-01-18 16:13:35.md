```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <string>

using namespace std;

template <typename T>
class DisjointSet {
public:
    DisjointSet(int n) : parent(n), size(n, 1) {}

    int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]);
        }
        return parent[x];
    }

    void unite(int x, int y) {
        int x_root = find(x);
        int y_root = find(y);
        if (x_root == y_root) {
            return;
        }
        if (size[x_root] < size[y_root]) {
            parent[x_root] = y_root;
            size[y_root] += size[x_root];
        } else {
            parent[y_root] = x_root;
            size[x_root] += size[y_root];
        }
    }

private:
    vector<int> parent;
    vector<int> size;
};

int main() {
    // Read input
    int n, m;
    cin >> n >> m;
    vector<pair<int, int>> edges;
    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        edges.push_back({u, v});
    }

    // Sort edges by weight
    sort(edges.begin(), edges.end(), [](const pair<int, int>& a, const pair<int, int>& b) {
        return a.second < b.second;
    });

    // Create disjoint set union data structure
    DisjointSet dsu(n + 1);

    // Initialize variables
    int total_weight = 0;
    vector<pair<int, int>> mst_edges;

    // Process edges
    for (const auto& edge : edges) {
        int u = edge.first;
        int v = edge.second;
        int weight = edge.second;

        if (dsu.find(u) != dsu.find(v)) {
            // Add edge to MST
            mst_edges.push_back(edge);

            // Update total weight
            total_weight += weight;

            // Unite vertices in disjoint set union data structure
            dsu.unite(u, v);
        }
    }

    // Output MST
    cout << total_weight << endl;
    for (const auto& edge : mst_edges) {
        cout << edge.first << " " << edge.second << endl;
    }

    return 0;
}
```

This code finds the minimum spanning tree of a graph using Kruskal's algorithm.

The graph is represented as a list of edges, where each edge is a pair of vertices and a weight. The code first sorts the edges by weight, then uses a disjoint set union data structure to keep track of which vertices are connected.

The algorithm works by iterating through the edges in sorted order, and adding each edge to the MST if it does not create a cycle. If an edge does create a cycle, it is discarded.

The code keeps track of the total weight of the MST and the edges that are in the MST.

At the end, the code outputs the total weight of the MST and the edges that are in the MST.