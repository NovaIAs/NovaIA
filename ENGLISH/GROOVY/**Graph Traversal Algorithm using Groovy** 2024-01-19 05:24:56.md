```groovy
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

// Define a class to represent a node in a graph. Each node has a name and a list of neighbors.
class Node {
  String name
  List<Node> neighbors = []

  // This constructor takes a name as a parameter and sets the name of the node.
  Node(String name) {
    this.name = name
  }

  // This method adds a neighbor to the node.
  void addNeighbor(Node neighbor) {
    neighbors << neighbor
  }

  // This method returns a string representation of the node.
  @Override
  String toString() {
    return "Node{name='$name', neighbors=$neighbors}"
  }
}

// Define a class to represent a graph. A graph has a list of nodes and a list of edges.
class Graph {
  List<Node> nodes = []
  List<Edge> edges = []

  // This constructor takes a list of nodes and a list of edges as parameters and sets the nodes and edges of the graph.
  Graph(List<Node> nodes, List<Edge> edges) {
    this.nodes = nodes
    this.edges = edges
  }

  // This method adds a node to the graph.
  void addNode(Node node) {
    nodes << node
  }

  // This method adds an edge to the graph.
  void addEdge(Edge edge) {
    edges << edge
  }

  // This method returns a string representation of the graph.
  @Override
  String toString() {
    return "Graph{nodes=$nodes, edges=$edges}"
  }
}

// Define a class to represent an edge in a graph. An edge has a source node, a target node, and a weight.
class Edge {
  Node source
  Node target
  int weight

  // This constructor takes a source node, a target node, and a weight as parameters and sets the source, target, and weight of the edge.
  Edge(Node source, Node target, int weight) {
    this.source = source
    this.target = target
    this.weight = weight
  }

  // This method returns a string representation of the edge.
  @Override
  String toString() {
    return "Edge{source=$source, target=$target, weight=$weight}"
  }
}

// Define a class to represent a graph traversal algorithm. The algorithm uses a queue to store the nodes that have been visited and a set to store the nodes that have been processed.
class GraphTraversalAlgorithm {
  Graph graph
  Queue<Node> queue = new LinkedList<>()
  Set<Node> processed = new HashSet<>()

  // This constructor takes a graph as a parameter and sets the graph of the algorithm.
  GraphTraversalAlgorithm(Graph graph) {
    this.graph = graph
  }

  // This method performs a breadth-first search of the graph starting from the given node.
  void breadthFirstSearch(Node startNode) {
    queue.add(startNode)
    processed.add(startNode)

    while (!queue.isEmpty()) {
      Node currentNode = queue.remove()

      // Visit the current node.

      for (Node neighbor : currentNode.neighbors) {
        if (!processed.contains(neighbor