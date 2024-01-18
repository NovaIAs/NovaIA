```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <string>
#include <sstream>
#include <fstream>

using namespace std;

// Define a struct to represent a node in a graph
struct Node {
  string name;
  vector<Node*> neighbors;
};

// Define a function to read a graph from a file
vector<Node*> readGraphFromFile(string filename) {
  // Create a vector of nodes to store the graph
  vector<Node*> graph;

  // Open the file
  ifstream file(filename);
  if (!file.is_open()) {
    throw runtime_error("Could not open file " + filename);
  }

  // Read the first line of the file, which contains the number of nodes
  int numNodes;
  file >> numNodes;

  // Create a node for each line of the file
  for (int i = 0; i < numNodes; i++) {
    // Read the name of the node
    string name;
    file >> name;

    // Create a new node
    Node* node = new Node;
    node->name = name;

    // Read the neighbors of the node
    int numNeighbors;
    file >> numNeighbors;

    for (int j = 0; j < numNeighbors; j++) {
      // Read the name of the neighbor
      string neighborName;
      file >> neighborName;

      // Find the node with the given name
      Node* neighbor = findNodeByName(graph, neighborName);

      // Add the neighbor to the list of neighbors
      node->neighbors.push_back(neighbor);
    }

    // Add the node to the vector of nodes
    graph.push_back(node);
  }

  // Close the file
  file.close();

  // Return the graph
  return graph;
}

// Define a function to find a node in a graph by name
Node* findNodeByName(vector<Node*> graph, string name) {
  // Iterate over the nodes in the graph
  for (Node* node : graph) {
    // If the name of the node matches the given name, return the node
    if (node->name == name) {
      return node;
    }
  }

  // If the node was not found, return nullptr
  return nullptr;
}

// Define a function to print a graph to the console
void printGraph(vector<Node*> graph) {
  // Iterate over the nodes in the graph
  for (Node* node : graph) {
    // Print the name of the node
    cout << node->name << endl;

    // Print the neighbors of the node
    for (Node* neighbor : node->neighbors) {
      cout << "\t" << neighbor->name << endl;
    }
  }
}

// Define a function to find all paths between two nodes in a graph
vector<vector<Node*>> findPaths(vector<Node*> graph, Node* start, Node* end) {
  // Create a vector of paths to store the results
  vector<vector<Node*>> paths;

  // Create a stack to store the current path
  vector<Node*> currentPath;

  // Push the start node onto the stack
  currentPath.push_back(start);

  // While the stack is not empty
  while (!currentPath.empty()) {
    // Get the last node on the stack
    Node* currentNode = currentPath.back();

    // If the last node is the end node, then we have found a path
    if (currentNode == end) {
      // Copy the current path into the vector of paths
      paths.push_back(currentPath);

      // Pop the last node off the stack
      currentPath.pop_back();

      // Continue to the next path
      continue;
    }

    // Iterate over the neighbors of the current node
    for (Node* neighbor : currentNode->neighbors) {
      // If the neighbor is not already on the stack, then add it to the stack
      if (find(currentPath.begin(), currentPath.end(), neighbor) == currentPath.end()) {
        // Push the neighbor onto the stack
        currentPath.push_back(neighbor);

        // Continue to the next neighbor
        continue;
      }
    }

    // If we have tried all of the neighbors of the current node and none of them led to a path, then pop the current node off the stack
    currentPath.pop_back();
  }

  // Return the vector of paths
  return paths;
}

// Define a function to find the shortest path between two nodes in a graph
vector<Node*> findShortestPath(vector<Node*> graph, Node* start, Node* end) {
  // Create a queue to store the nodes to visit
  queue<Node*> queue;

  // Create a map to store the distance of each node from the start node
  unordered_map<Node*, int> distance;

  // Create a map to store the parent of each node in the shortest path
  unordered_map<Node*, Node*> parent;

  // Set the distance of the start node to 0
  distance[start] = 0;

  // Push the start node onto the queue
  queue.push(start);

  // While the queue is not empty
  while (!queue.empty()) {
    // Get the next node from the queue
    Node* currentNode = queue.front();

    // Pop the next node off the queue
    queue.pop();

    // Iterate over the neighbors of the current node
    for (Node* neighbor : currentNode->neighbors) {
      // If the distance of the neighbor is infinity, then set it to the distance of the current node plus 1
      if (distance.find(neighbor) == distance.end()) {
        distance[neighbor] = distance[currentNode] + 1;

        // Set the parent of the neighbor to the current node
        parent[neighbor] = currentNode;

        // Push the neighbor onto the queue
        queue.push(neighbor);
      }
    }
  }

  // If the distance of the end node is infinity, then there is no path between the start node and the end node
  if (distance[end] == INT_MAX) {
    return {};
  }

  // Reconstruct the