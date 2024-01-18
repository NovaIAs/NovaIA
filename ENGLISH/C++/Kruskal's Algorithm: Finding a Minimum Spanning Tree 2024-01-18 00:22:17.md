```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <string>

using namespace std;

class Graph {
public:
    Graph(int numVertices) {
        adjList.resize(numVertices);
    }

    void addEdge(int u, int v) {
        adjList[u].push_back(v);
        adjList[v].push_back(u);
    }

    vector<vector<int>> getAdjList() {
        return adjList;
    }

private:
    vector<vector<int>> adjList;
};

class DisjointSet {
public:
    DisjointSet(int numVertices) {
        parent.resize(numVertices);
        rank.resize(numVertices);
        for (int i = 0; i < numVertices; i++) {
            parent[i] = i;
            rank[i] = 0;
        }
    }

    int find(int u) {
        if (parent[u] != u) {
            parent[u] = find(parent[u]);
        }
        return parent[u];
    }

    void union(int u, int v) {
        int uRoot = find(u);
        int vRoot = find(v);
        if (uRoot == vRoot) {
            return;
        }
        if (rank[uRoot] < rank[vRoot]) {
            parent[uRoot] = vRoot;
        } else if (rank[uRoot] > rank[vRoot]) {
            parent[vRoot] = uRoot;
        } else {
            parent[vRoot] = uRoot;
            rank[uRoot]++;
        }
    }

private:
    vector<int> parent;
    vector<int> rank;
};

class KruskalMST {
public:
    KruskalMST(Graph& graph) {
        this->graph = graph;
        numVertices = graph.getAdjList().size();
        edges = getEdges();
    }

    vector<pair<int, int>> getMST() {
        sort(edges.begin(), edges.end(), [](const pair<int, int>& edge1, const pair<int, int>& edge2) {
            return edge1.second < edge2.second;
        });

        DisjointSet disjointSet(numVertices);
        vector<pair<int, int>> mstEdges;

        for (const auto& edge : edges) {
            int u = edge.first;
            int v = edge.second;
            if (disjointSet.find(u) != disjointSet.find(v)) {
                mstEdges.push_back(edge);
                disjointSet.union(u, v);
            }
        }

        return mstEdges;
    }

private:
    Graph graph;
    int numVertices;
    vector<pair<int, int>> edges;

    vector<pair<int, int>> getEdges() {
        vector<pair<int, int>> edges;
        vector<vector<int>> adjList = graph.getAdjList();
        for (int i = 0; i < numVertices; i++) {
            for (int j = i + 1; j < numVertices; j++) {
                if (find(adjList[i].begin(), adjList[i].end(), j) != adjList[i].end()) {
                    edges.push_back({i, j});
                }
            }
        }
        return edges;
    }
};

int main() {
    int numVertices, numEdges;
    cin >> numVertices >> numEdges;

    Graph graph(numVertices);

    for (int i = 0; i < numEdges; i++) {
        int u, v;
        cin >> u >> v;
        graph.addEdge(u, v);
    }

    KruskalMST mst(graph);

    vector<pair<int, int>> mstEdges = mst.getMST();

    for (const auto& edge : mstEdges) {
        cout << edge.first << " " << edge.second << endl;
    }

    return 0;
}
```

This code implements Kruskal's algorithm to find the minimum spanning tree of a graph. The code first reads the number of vertices and edges from the input. Then, it creates a graph object and adds the edges to the graph. Next, it creates a KruskalMST object, which takes the graph as an argument. The KruskalMST object contains a method called getMST(), which returns the minimum spanning tree of the graph. Finally, the code prints the edges of the minimum spanning tree to the console.

Here is a breakdown of the code:

* The `Graph` class represents a graph. It has a constructor that takes the number of vertices as an argument. The class also has methods to add edges and to get the adjacency list of the graph.
* The `DisjointSet` class represents a disjoint set data structure. It has a constructor that takes the number of elements as an argument. The class also has methods to find the parent of an element and to union two sets.
* The `KruskalMST` class implements Kruskal's algorithm to find the minimum spanning tree of a graph. It has a constructor that takes a graph object as an argument. The class also has a method called `getMST()` that returns the minimum spanning tree of the graph.
* The `main()` function reads the number of vertices and edges from the input. Then, it creates a graph object and adds the edges to the graph. Next, it creates a KruskalMST object, which takes the graph as an argument. The KruskalMST object contains a method called `getMST()`, which returns the minimum spanning tree of the graph. Finally, the code prints the edges of the minimum spanning tree to the console.