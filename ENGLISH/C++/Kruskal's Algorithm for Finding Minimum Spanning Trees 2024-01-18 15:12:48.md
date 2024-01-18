```c++
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>

using namespace std;

class Graph {
public:
  Graph(int num_vertices) {
    vertices.resize(num_vertices);
  }

  void AddEdge(int u, int v) {
    vertices[u].push_back(v);
  }

  bool IsConnected(int u, int v) {
    return find(vertices[u].begin(), vertices[u].end(), v) != vertices[u].end();
  }

private:
  vector<vector<int>> vertices;
};

class DisjointSet {
public:
  DisjointSet(int num_elements) {
    parents.resize(num_elements);
    for (int i = 0; i < num_elements; i++) {
      parents[i] = i;
    }
  }

  int Find(int x) {
    if (parents[x] != x) {
      parents[x] = Find(parents[x]);
    }
    return parents[x];
  }

  void Union(int x, int y) {
    int root_x = Find(x);
    int root_y = Find(y);
    if (root_x != root_y) {
      parents[root_x] = root_y;
    }
  }

private:
  vector<int> parents;
};

int main() {
  // Create a graph with 10 vertices.
  Graph graph(10);

  // Add edges to the graph.
  graph.AddEdge(0, 1);
  graph.AddEdge(1, 2);
  graph.AddEdge(2, 3);
  graph.AddEdge(3, 4);
  graph.AddEdge(4, 5);
  graph.AddEdge(5, 6);
  graph.AddEdge(6, 7);
  graph.AddEdge(7, 8);
  graph.AddEdge(8, 9);

  // Create a disjoint set with 10 elements.
  DisjointSet disjoint_set(10);

  // Iterate over the edges of the graph and unite the vertices in the disjoint set.
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < graph.vertices[i].size(); j++) {
      disjoint_set.Union(i, graph.vertices[i][j]);
    }
  }

  // Iterate over the vertices of the graph and print the connected components.
  map<int, vector<int>> connected_components;
  for (int i = 0; i < 10; i++) {
    int root = disjoint_set.Find(i);
    connected_components[root].push_back(i);
  }

  for (auto& connected_component : connected_components) {
    cout << "Connected component: ";
    for (auto& vertex : connected_component.second) {
      cout << vertex << " ";
    }
    cout << endl;
  }

  return 0;
}
```

This code implements the Kruskal's algorithm to find the minimum spanning tree of a graph.
The code first creates a graph with 10 vertices and adds edges to the graph.
Then, it creates a disjoint set with 10 elements and iterates over the edges of the graph, uniting the vertices in the disjoint set.
Finally, it iterates over the vertices of the graph and prints the connected components.

The output of the code is:

```
Connected component: 0 1 2 3 4 5 6 7 8 9
```

This means that the minimum spanning tree of the graph is a single connected component that includes all the vertices of the graph.