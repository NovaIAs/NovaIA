```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <sstream>
#include <fstream>
#include <iterator>
#include <iomanip>
#include <numeric>
#include <math.h>

using namespace std;

// Define a custom data structure to represent a graph
struct Edge {
  int source;
  int destination;
  int weight;
};

struct Graph {
  vector<vector<Edge>> adjacency_list;
  int num_vertices;
};

// Function to create a new graph with the given number of vertices
Graph create_graph(int num_vertices) {
  Graph graph;
  graph.num_vertices = num_vertices;
  graph.adjacency_list.resize(num_vertices);
  return graph;
}

// Function to add an edge to the graph
void add_edge(Graph& graph, int source, int destination, int weight) {
  Edge edge;
  edge.source = source;
  edge.destination = destination;
  edge.weight = weight;
  graph.adjacency_list[source].push_back(edge);
}

// Function to print the adjacency list of the graph
void print_graph(const Graph& graph) {
  for (int i = 0; i < graph.num_vertices; i++) {
    cout << "Vertex " << i << ": ";
    for (const Edge& edge : graph.adjacency_list[i]) {
      cout << "(" << edge.destination << ", " << edge.weight << ") ";
    }
    cout << endl;
  }
}

// Function to perform a depth-first search of the graph
void depth_first_search(const Graph& graph, int start_vertex) {
  // Create a vector to keep track of visited vertices
  vector<bool> visited(graph.num_vertices, false);

  // Create a stack to keep track of vertices to visit
  stack<int> stack;

  // Push the start vertex onto the stack
  stack.push(start_vertex);

  // While the stack is not empty, pop the top vertex and visit it
  while (!stack.empty()) {
    int vertex = stack.top();
    stack.pop();

    // If the vertex has not been visited, mark it as visited and print it
    if (!visited[vertex]) {
      visited[vertex] = true;
      cout << vertex << " ";

      // Push all of the vertex's unvisited neighbors onto the stack
      for (const Edge& edge : graph.adjacency_list[vertex]) {
        if (!visited[edge.destination]) {
          stack.push(edge.destination);
        }
      }
    }
  }
}

// Function to perform a breadth-first search of the graph
void breadth_first_search(const Graph& graph, int start_vertex) {
  // Create a vector to keep track of visited vertices
  vector<bool> visited(graph.num_vertices, false);

  // Create a queue to keep track of vertices to visit
  queue<int> queue;

  // Push the start vertex onto the queue
  queue.push(start_vertex);

  // While the queue is not empty, pop the front vertex and visit it
  while (!queue.empty()) {
    int vertex = queue.front();
    queue.pop();

    // If the vertex has not been visited, mark it as visited and print it
    if (!visited[vertex]) {
      visited[vertex] = true;
      cout << vertex << " ";

      // Push all of the vertex's unvisited neighbors onto the queue
      for (const Edge& edge : graph.adjacency_list[vertex]) {
        if (!visited[edge.destination]) {
          queue.push(edge.destination);
        }
      }
    }
  }
}

// Function to find the shortest path between two vertices using Dijkstra's algorithm
vector<int> dijkstra(const Graph& graph, int start_vertex, int end_vertex) {
  // Create a vector to store the shortest distances from the start vertex to all other vertices
  vector<int> distances(graph.num_vertices, INT_MAX);

  // Set the distance from the start vertex to itself to 0
  distances[start_vertex] = 0;

  // Create a set to keep track of vertices that have been visited
  set<int> visited;

  // While the set of visited vertices does not contain the end vertex
  while (visited.find(end_vertex) == visited.end()) {
    // Find the unvisited vertex with the smallest distance from the start vertex
    int min_distance_vertex = -1;
    int min_distance = INT_MAX;
    for (int i = 0; i < graph.num_vertices; i++) {
      if (!visited.count(i) && distances[i] < min_distance) {
        min_distance_vertex = i;
        min_distance = distances[i];
      }
    }

    // If no unvisited vertices remain, then there is no path from the start vertex to the end vertex
    if (min_distance_vertex == -1) {
      return vector<int>();
    }

    // Visit the vertex and update the distances to its neighbors
    visited.insert(min_distance_vertex);
    for (const Edge& edge : graph.adjacency_list[min_distance_vertex]) {
      if (!visited.count(edge.destination) && distances[min_distance_vertex] + edge.weight < distances[edge.destination]) {
        distances[edge.destination] = distances[min_distance_vertex] + edge.weight;
      }
    }
  }

  // If the distance from the start vertex to the end vertex is still INT_MAX, then there is no path between them
  if (distances[end_vertex] == INT_MAX) {
    return vector<int>();
  }

  // Reconstruct the shortest path from the start vertex to the end vertex
  vector<int> path;
  int current_vertex = end_vertex;
  while (current_vertex != start_vertex) {
    path.push_back(current_vertex);
    for (const Edge& edge : graph.adjacency_list[current_vertex]) {
      if (distances[current_vertex] - edge.weight == distances[edge.source]) {
        current_vertex = edge.source;
        break;
      }
    }
  }
  path.push_back(start_vertex);
  reverse(path.begin(), path.end());
  return path;
}

// Function to find the minimum spanning tree of a graph using Kruskal's algorithm
vector<Edge> kruskal(const Graph& graph) {
  // Create a set to store the edges in the minimum spanning tree
  set<Edge> mst;

  // Create a disjoint-set data structure to keep track of which vertices are connected
  DisjointSetUnion dsu(graph.num_vertices);

  // Sort the edges in ascending order of weight
  vector<Edge> edges;
  for (int i = 0; i < graph.num_vertices; i++) {
    for (const Edge& edge : graph.adjacency_list[i]) {
      edges.push_back(edge);
    }
  }
  sort(edges.begin(), edges.end(), [](const Edge& a, const Edge& b) { return a.weight < b.weight; });

  // Add edges to the minimum spanning tree until all vertices are connected
  for (const Edge& edge : edges) {
    if (dsu.find(edge.source) != dsu.find(edge.destination)) {
      mst.insert(edge);
      dsu.union(edge.source, edge.destination);
    }
  }

  // Return the edges in the minimum spanning tree
  return vector<Edge>(mst.begin(), mst.end());
}

// Main function
int main() {
  // Create a graph with 9 vertices
  Graph graph = create_graph(9);

  // Add edges to the graph
  add_edge(graph, 0, 1, 4);
  add_edge(graph, 0, 6, 7);
  add_edge(graph, 1, 2, 9);
  add_edge(graph, 1, 6, 14);
  add_edge(graph, 2, 3, 2);
  add_edge(graph, 2, 5, 1);
  add_edge(graph, 3, 4, 6);
  add_edge(graph, 4, 5, 10);
  add_edge(graph, 5, 6, 2);
  add_edge(graph, 6, 7, 1);
