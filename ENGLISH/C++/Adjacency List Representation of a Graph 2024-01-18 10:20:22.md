```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <string>

using namespace std;

// Define the maximum number of nodes in the graph
#define MAX_NODES 100

// Define the maximum number of edges in the graph
#define MAX_EDGES 1000

// Define the data type for the graph
struct Graph {
  // Define the number of nodes in the graph
  int num_nodes;

  // Define the number of edges in the graph
  int num_edges;

  // Define the adjacency list for the graph
  vector<vector<int>> adj_list;

  // Define the constructor for the graph
  Graph(int num_nodes, int num_edges) {
    this->num_nodes = num_nodes;
    this->num_edges = num_edges;

    // Initialize the adjacency list
    adj_list.resize(num_nodes + 1);
  }

  // Define the function to add an edge to the graph
  void add_edge(int u, int v) {
    // Add the edge to the adjacency list
    adj_list[u].push_back(v);

    // Increment the number of edges in the graph
    num_edges++;
  }

  // Define the function to print the graph
  void print_graph() {
    // Iterate over the nodes in the graph
    for (int i = 1; i <= num_nodes; i++) {
      // Print the node and its adjacent nodes
      cout << "Node " << i << ": ";
      for (int j = 0; j < adj_list[i].size(); j++) {
        cout << adj_list[i][j] << " ";
      }
      cout << endl;
    }
  }
};

// Define the main function
int main() {
  // Create a graph with 5 nodes and 6 edges
  Graph graph(5, 6);

  // Add the edges to the graph
  graph.add_edge(1, 2);
  graph.add_edge(1, 3);
  graph.add_edge(2, 4);
  graph.add_edge(2, 5);
  graph.add_edge(3, 4);
  graph.add_edge(4, 5);

  // Print the graph
  graph.print_graph();

  // Return 0 to indicate successful execution
  return 0;
}
```

Explanation:

* The code defines a `Graph` struct that represents a graph data structure.
* The `Graph` struct has three attributes: `num_nodes`, `num_edges`, and `adj_list`.
* `num_nodes` stores the number of nodes in the graph.
* `num_edges` stores the number of edges in the graph.
* `adj_list` is a vector of vectors that stores the adjacency list representation of the graph.
* The `Graph` struct also has a constructor that takes the number of nodes and edges as parameters and initializes the graph accordingly.
* The `add_edge` function adds an edge to the graph by adding the edge to the adjacency list and incrementing the number of edges in the graph.
* The `print_graph` function prints the graph by iterating over the nodes in the graph and printing the node and its adjacent nodes.
* The `main` function creates a graph with 5 nodes and 6 edges, adds the edges to the graph, and then prints the graph.