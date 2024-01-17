```dart
// Import necessary libraries.
import 'dart:collection';
import 'dart:io';

// Define a custom data structure to store a graph.
class Graph<T> {
  // Use a HashMap to store the vertices and their connections.
  Map<T, Set<T>> _vertices = HashMap<T, Set<T>>();

  // Add a new vertex to the graph.
  void addVertex(T vertex) {
    if (!_vertices.containsKey(vertex)) {
      _vertices[vertex] = Set<T>();
    }
  }

  // Add an edge between two vertices.
  void addEdge(T from, T to) {
    if (_vertices.containsKey(from) && _vertices.containsKey(to)) {
      _vertices[from]!.add(to);
      _vertices[to]!.add(from);
    }
  }

  // Depth-First Search (DFS) algorithm to traverse the graph.
  List<T> depthFirstSearch(T start) {
    // Initialize a stack and a set to keep track of visited vertices.
    List<T> stack = [start];
    Set<T> visited = Set<T>();

    // While there are vertices in the stack, pop them and add their neighbors.
    while (stack.isNotEmpty) {
      T current = stack.removeLast();
      if (!visited.contains(current)) {
        visited.add(current);
        stack.addAll(_vertices[current]!);
      }
    }

    // Return the list of visited vertices.
    return visited.toList();
  }

  // Breadth-First Search (BFS) algorithm to traverse the graph.
  List<T> breadthFirstSearch(T start) {
    // Initialize a queue and a set to keep track of visited vertices.
    Queue<T> queue = Queue<T>();
    Set<T> visited = Set<T>();

    // Add the starting vertex to the queue and mark it as visited.
    queue.add(start);
    visited.add(start);

    // While there are vertices in the queue, dequeue them and add their neighbors.
    while (queue.isNotEmpty) {
      T current = queue.removeFirst();
      for (T neighbor in _vertices[current]!) {
        if (!visited.contains(neighbor)) {
          visited.add(neighbor);
          queue.add(neighbor);
        }
      }
    }

    // Return the list of visited vertices.
    return visited.toList();
  }
}

// Define a main function to test the graph data structure.
void main() {
  // Create a new graph.
  Graph<String> graph = Graph<String>();

  // Add some vertices.
  graph.addVertex('A');
  graph.addVertex('B');
  graph.addVertex('C');
  graph.addVertex('D');
  graph.addVertex('E');

  // Add some edges.
  graph.addEdge('A', 'B');
  graph.addEdge('B', 'C');
  graph.addEdge('C', 'D');
  graph.addEdge('D', 'E');
  graph.addEdge('E', 'A');

  // Perform depth-first search and print the result.
  print('Depth-First Search: ${graph.depthFirstSearch('A')}');

  // Perform breadth-first search and print the result.
  print('Breadth-First Search: ${graph.breadthFirstSearch('A')}');
}
```