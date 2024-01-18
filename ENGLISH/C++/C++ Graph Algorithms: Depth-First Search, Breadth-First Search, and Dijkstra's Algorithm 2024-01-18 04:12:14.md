```c++
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <functional>

using namespace std;

// Define a custom comparator function for sorting pairs by their second element
bool compare_by_second(const pair<int, int>& a, const pair<int, int>& b) {
  return a.second < b.second;
}

// Define a custom hash function for a pair of integers
struct pair_hash {
  size_t operator()(const pair<int, int>& p) const {
    return hash<int>()(p.first) ^ hash<int>()(p.second);
  }
};

// Define a custom equality comparison function for a pair of integers
struct pair_equal {
  bool operator()(const pair<int, int>& a, const pair<int, int>& b) const {
    return a.first == b.first && a.second == b.second;
  }
};

// Define a class to represent a graph
class Graph {
public:
  // Constructor
  Graph(int num_vertices) {
    // Initialize the adjacency list
    adj_list.resize(num_vertices);
  }

  // Add an edge to the graph
  void add_edge(int u, int v) {
    // Add the edge to the adjacency list
    adj_list[u].push_back(v);
    adj_list[v].push_back(u);
  }

  // Perform a depth-first search on the graph
  void dfs(int start_vertex) {
    // Create a stack to store the vertices to visit
    stack<int> stack;

    // Create a set to store the visited vertices
    set<int> visited;

    // Push the start vertex onto the stack
    stack.push(start_vertex);

    // While the stack is not empty
    while (!stack.empty()) {
      // Pop the top vertex from the stack
      int vertex = stack.top();
      stack.pop();

      // If the vertex has not been visited
      if (visited.find(vertex) == visited.end()) {
        // Mark the vertex as visited
        visited.insert(vertex);

        // Print the vertex
        cout << vertex << " ";

        // Push all the adjacent vertices onto the stack
        for (int neighbor : adj_list[vertex]) {
          if (visited.find(neighbor) == visited.end()) {
            stack.push(neighbor);
          }
        }
      }
    }
  }

  // Perform a breadth-first search on the graph
  void bfs(int start_vertex) {
    // Create a queue to store the vertices to visit
    queue<int> queue;

    // Create a set to store the visited vertices
    set<int> visited;

    // Enqueue the start vertex
    queue.push(start_vertex);

    // While the queue is not empty
    while (!queue.empty()) {
      // Dequeue the front vertex from the queue
      int vertex = queue.front();
      queue.pop();

      // If the vertex has not been visited
      if (visited.find(vertex) == visited.end()) {
        // Mark the vertex as visited
        visited.insert(vertex);

        // Print the vertex
        cout << vertex << " ";

        // Enqueue all the adjacent vertices
        for (int neighbor : adj_list[vertex]) {
          if (visited.find(neighbor) == visited.end()) {
            queue.push(neighbor);
          }
        }
      }
    }
  }

  // Find the shortest path between two vertices using Dijkstra's algorithm
  vector<int> dijkstra(int start_vertex, int end_vertex) {
    // Create a map to store the shortest distances from the start vertex to all other vertices
    map<int, int> distances;

    // Create a set to store the visited vertices
    set<int> visited;

    // Initialize the distances map
    for (int i = 0; i < adj_list.size(); i++) {
      distances[i] = INT_MAX;
    }

    // Set the distance from the start vertex to itself to 0
    distances[start_vertex] = 0;

    // While the end vertex has not been visited
    while (visited.find(end_vertex) == visited.end()) {
      // Find the vertex with the smallest distance that has not been visited
      int min_vertex = -1;
      int min_distance = INT_MAX;
      for (int i = 0; i < adj_list.size(); i++) {
        if (visited.find(i) == visited.end() && distances[i] < min_distance) {
          min_vertex = i;
          min_distance = distances[i];
        }
      }

      // If there are no more unvisited vertices, break from the loop
      if (min_vertex == -1) {
        break;
      }

      // Mark the vertex as visited
      visited.insert(min_vertex);

      // Update the distances of the adjacent vertices
      for