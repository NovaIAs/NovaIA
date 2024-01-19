```c++
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <sstream>
#include <fstream>

using namespace std;

// Define a class to represent a directed graph
class Graph {
public:
  // Constructor
  Graph(int num_vertices): num_vertices(num_vertices) {
    // Initialize the adjacency list
    adj_list.resize(num_vertices);
  }

  // Add an edge to the graph
  void add_edge(int u, int v, int weight) {
    // Check if the vertices are valid
    if (u < 0 || u >= num_vertices || v < 0 || v >= num_vertices) {
      throw invalid_argument("Invalid vertices");
    }

    // Add the edge to the adjacency list
    adj_list[u].push_back({v, weight});
  }

  // Perform a depth-first search on the graph
  void dfs(int start_vertex) {
    // Check if the start vertex is valid
    if (start_vertex < 0 || start_vertex >= num_vertices) {
      throw invalid_argument("Invalid start vertex");
    }

    // Initialize the visited array
    vector<bool> visited(num_vertices, false);

    // Perform the depth-first search
    dfs_visit(start_vertex, visited);
  }

  // Perform a breadth-first search on the graph
  void bfs(int start_vertex) {
    // Check if the start vertex is valid
    if (start_vertex < 0 || start_vertex >= num_vertices) {
      throw invalid_argument("Invalid start vertex");
    }

    // Initialize the queue and visited array
    queue<int> q;
    vector<bool> visited(num_vertices, false);

    // Add the start vertex to the queue and mark it as visited
    q.push(start_vertex);
    visited[start_vertex] = true;

    // While the queue is not empty, continue the breadth-first search
    while (!q.empty()) {
      // Dequeue the front element from the queue
      int current_vertex = q.front();
      q.pop();

      // Visit the current vertex
      cout << "Visiting vertex " << current_vertex << endl;

      // Enqueue the neighbors of the current vertex and mark them as visited
      for (auto edge : adj_list[current_vertex]) {
        int neighbor = edge.first;
        if (!visited[neighbor]) {
          q.push(neighbor);
          visited[neighbor] = true;
        }
      }
    }
  }

  // Find the shortest path between two vertices using Dijkstra's algorithm
  vector<int> dijkstra(int start_vertex, int end_vertex) {
    // Initialize the distance array
    vector<int> distance(num_vertices, INT_MAX);
    distance[start_vertex] = 0;

    // Initialize the visited array
    vector<bool> visited(num_vertices, false);

    // Initialize the priority queue
    priority_queue<pair<int, int>> pq;
    pq.push({0, start_vertex});

    // While the priority queue is not empty, continue the algorithm
    while (!pq.empty()) {
      // Get the current vertex and its distance from the priority queue
      int current_vertex = pq.top().second;
      int current_distance = -pq.top().first;
      pq.pop();

      // If the current vertex is the end vertex, stop the algorithm
      if (current_vertex == end_vertex) {
        break;
      }

      // If the current vertex has already been visited, continue
      if (visited[current_vertex]) {
        continue;
      }

      // Mark the current vertex as visited
      visited[current_vertex] = true;

      // Relax the edges from the current vertex
      for (auto edge : adj_list[current_vertex]) {
        int neighbor = edge.first;
        int weight = edge.second;

        // If the distance to the neighbor is greater than the distance through the current vertex, update the distance to the neighbor
        if (distance[neighbor] > current_distance + weight) {
          distance[neighbor] = current_distance + weight;
          pq.push({-distance[neighbor], neighbor});
        }
      }
    }

    // Return the shortest path from the start vertex to the end vertex
    vector<int> path;
    int current_vertex = end_vertex;
    while (current_vertex != start_vertex) {
      path.push_back(current_vertex);

      // Find the predecessor of the current vertex in the shortest path
      int predecessor = -1;
      for (auto edge : adj_list[current_vertex]) {
        int neighbor = edge.first;
        int weight = edge.second;

        if (distance[current_vertex] == distance[neighbor] + weight) {
          predecessor = neighbor;
          break;
        }
      }

      current_vertex = predecessor;
    }
    path.push_back(start_vertex);

    // Reverse the path to get the correct order
    reverse(path.begin(), path.end());

    return path;
  }

private:
  // The number of vertices in the graph
  int num_vertices;

  // The adjacency list representing the graph
  vector<vector<pair<int, int>>> adj_list;

  // Perform a depth-first search on the graph starting from a given vertex
  void dfs_visit(int vertex, vector<bool>& visited) {
    // Mark the vertex as visited
    visited[vertex] = true;

    // Print the vertex
    cout << "Visiting vertex " << vertex << endl;

    // Visit the neighbors of the vertex
    for (auto edge : adj_list[vertex]) {
      int neighbor = edge.first;

      // If the neighbor has not been visited, recursively visit it
      if (!visited[neighbor]) {
        dfs_visit(neighbor, visited);
      }
    }
  }
};

// Read a graph from a file
Graph read_graph_from_file(string filename) {
  // Open the file
  ifstream file(filename);
  if (!file.is_open()) {
    throw runtime_error("Could not open file");
  }

  // Read the number of vertices and edges
  int num_vertices, num_edges;
  file >> num_vertices >> num_edges;

  // Create a graph with the specified number of vertices
  Graph graph(num_vertices);

  // Read the edges
  for (int i = 0; i < num_edges; i++) {
    int u, v, w;
    file >> u >> v >> w;

    // Add the edge to the graph
    graph.add_edge(u, v, w);
  }

  // Close the file
  file.close();

  return graph;
}

// Print a graph to the console
void print_graph(Graph& graph) {
  // Print the number of vertices and edges
  cout << "Number of vertices: " << graph.num_vertices << endl;
  cout << "Number of edges: " << graph.adj_list.size() << endl;

  // Print the adjacency list
  for (int i = 0; i < graph.num_vertices; i++) {