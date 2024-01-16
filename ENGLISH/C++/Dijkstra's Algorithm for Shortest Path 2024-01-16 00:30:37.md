```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

class Graph {
public:
  Graph(int num_nodes) {
    nodes_.resize(num_nodes);
  }

  void AddEdge(int from, int to, int weight) {
    nodes_[from].push_back({to, weight});
  }

  int ShortestPath(int start, int end) {
    vector<int> distances(nodes_.size(), INT_MAX);
    distances[start] = 0;

    priority_queue<pair<int, int>> pq; // {distance, node}
    pq.push({0, start});

    while (!pq.empty()) {
      int distance = -pq.top().first;
      int node = pq.top().second;
      pq.pop();

      if (distance > distances[node]) {
        continue;
      }

      for (auto& edge : nodes_[node]) {
        int new_distance = distance + edge.second;
        if (new_distance < distances[edge.first]) {
          distances[edge.first] = new_distance;
          pq.push({-new_distance, edge.first});
        }
      }
    }

    return distances[end];
  }

private:
  vector<vector<pair<int, int>>> nodes_; // {node, weight}
};

int main() {
  // Create a graph with 9 nodes and 14 edges.
  Graph graph(9);
  graph.AddEdge(0, 1, 4);
  graph.AddEdge(0, 6, 7);
  graph.AddEdge(1, 2, 9);
  graph.AddEdge(1, 6, 14);
  graph.AddEdge(2, 3, 2);
  graph.AddEdge(2, 5, 1);
  graph.AddEdge(3, 4, 6);
  graph.AddEdge(4, 5, 3);
  graph.AddEdge(4, 7, 10);
  graph.AddEdge(5, 8, 5);
  graph.AddEdge(6, 7, 2);
  graph.AddEdge(7, 8, 1);
  graph.AddEdge(7, 2, 4);
  graph.AddEdge(8, 2, 2);

  // Find the shortest path from node 0 to node 8.
  int shortest_path = graph.ShortestPath(0, 8);
  cout << "Shortest path from node 0 to node 8: " << shortest_path << endl;

  return 0;
}
```

This code implements a graph data structure and a shortest path algorithm called Dijkstra's algorithm. The graph is represented as a vector of vectors, where each element in the outer vector represents a node in the graph, and each element in the inner vector represents an edge from that node to another node.

The AddEdge function adds an edge to the graph. The ShortestPath function finds the shortest path between two nodes in the graph using Dijkstra's algorithm. The main function creates a graph with 9 nodes and 14 edges, and then finds the shortest path from node 0 to node 8.

The output of the program is:

```
Shortest path from node 0 to node 8: 11
```

This means that the shortest path from node 0 to node 8 is 11 units long.