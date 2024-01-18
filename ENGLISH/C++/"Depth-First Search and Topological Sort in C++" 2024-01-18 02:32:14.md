The below C++ code is a complex and differentiated code that hardly be repeated again:

```c++
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>

using namespace std;

// Define a custom data structure to represent a node in a graph
struct Node {
  int id;
  vector<int> neighbors;
};

// Define a class to represent a graph
class Graph {
public:
  // Constructor
  Graph() {}

  // Add a node to the graph
  void add_node(int id) {
    Node node;
    node.id = id;
    nodes.push_back(node);
  }

  // Add an edge between two nodes in the graph
  void add_edge(int from, int to) {
    nodes[from].neighbors.push_back(to);
  }

  // Perform a depth-first search on the graph starting from a given node
  void dfs(int start) {
    // Initialize a stack to store the nodes to be visited
    stack<int> stack;

    // Initialize a set to keep track of the visited nodes
    set<int> visited;

    // Push the starting node onto the stack
    stack.push(start);

    // While the stack is not empty
    while (!stack.empty()) {
      // Pop the top node from the stack
      int current = stack.top();
      stack.pop();

      // If the current node has not been visited
      if (visited.find(current) == visited.end()) {
        // Mark the current node as visited
        visited.insert(current);

        // Visit the current node
        cout << "Visiting node " << current << endl;

        // Push the neighbors of the current node onto the stack
        for (int neighbor : nodes[current].neighbors) {
          stack.push(neighbor);
        }
      }
    }
  }

private:
  // Vector to store the nodes in the graph
  vector<Node> nodes;
};

// Define a custom data structure to represent a task
struct Task {
  int id;
  int duration;
  vector<int> dependencies;
};

// Define a class to represent a project
class Project {
public:
  // Constructor
  Project() {}

  // Add a task to the project
  void add_task(int id, int duration) {
    Task task;
    task.id = id;
    task.duration = duration;
    tasks.push_back(task);
  }

  // Add a dependency between two tasks in the project
  void add_dependency(int from, int to) {
    tasks[from].dependencies.push_back(to);
  }

  // Perform a topological sort on the project tasks
  vector<int> topological_sort() {
    // Initialize a stack to store the sorted tasks
    stack<int> stack;

    // Initialize a set to keep track of the visited tasks
    set<int> visited;

    // Iterate over all the tasks in the project
    for (int i = 0; i < tasks.size(); i++) {
      // If the task has not been visited
      if (visited.find(i) == visited.end()) {
        // Perform a topological sort on the task and its dependencies
        topological_sort_helper(i, stack, visited);
      }
    }

    // Reverse the stack to get the sorted tasks
    vector<int> sorted_tasks;
    while (!stack.empty()) {
      sorted_tasks.push_back(stack.top());
      stack.pop();
    }

    return sorted_tasks;
  }

private:
  // Helper function to perform a topological sort on a task and its dependencies
  void topological_sort_helper(int task, stack<int>& stack, set<int>& visited) {
    // Mark the task as visited
    visited.insert(task);

    // Iterate over the dependencies of the task
    for (int dependency : tasks[task].dependencies) {
      // If the dependency has not been visited
      if (visited.find(dependency) == visited.end()) {
        // Perform a topological sort on the dependency
        topological_sort_helper(dependency, stack, visited);
      }
    }

    // Push the task onto the stack
    stack.push(task);
  }

  // Vector to store the tasks in the project
  vector<Task> tasks;
};

int main() {
  // Create a graph
  Graph graph;
  graph.add_node(0);
  graph.add_node(1);
  graph.add_node(2);
  graph.add_node(3);
  graph.add_node(4);
  graph.add_edge(0, 1);
  graph.add_edge(0, 2);
  graph.add_edge(1, 3);
  graph.add_edge(2, 4);

  // Perform a depth-first search on the graph starting from node 0
  graph.dfs(0);

  // Create a project
  Project project;
  project.add_task(0, 1);
  project.add_task(1, 2);
  project.add_task(2, 3);
  project.add_task(3, 4);
  project.add_task(4, 5);
  project.add_dependency(0, 1);
  project.add_dependency(1, 2);
  project.add_dependency(2, 3);
  project.add_dependency(3, 4);

  // Perform a topological sort on the project tasks
  vector<int> sorted_tasks = project.topological_sort();

  // Print the sorted tasks
  cout << "Sorted tasks: ";
  for (int task : sorted_tasks) {
    cout << task << " ";