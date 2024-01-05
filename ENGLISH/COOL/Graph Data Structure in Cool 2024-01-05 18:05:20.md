```cool
class Graph {
  // fields
  nodes: List<Node>;
  edges: List<Edge>;

  // constructor
  new(nodes: List<Node>, edges: List<Edge>) {
    this.nodes = nodes;
    this.edges = edges;
  }

  // methods
  addNode(node: Node) {
    this.nodes.add(node);
  }

  addEdge(edge: Edge) {
    this.edges.add(edge);
  }

  findNode(name: String): Node? {
    for (node in this.nodes) {
      if (node.name == name) {
        return node;
      }
    }
    return null;
  }

  findEdge(source: Node, destination: Node): Edge? {
    for (edge in this.edges) {
      if (edge.source == source && edge.destination == destination) {
        return edge;
      }
    }
    return null;
  }

  // ... other methods ...
}

class Node {
  // fields
  name: String;
  value: Any;

  // constructor
  new(name: String, value: Any) {
    this.name = name;
    this.value = value;
  }

  // methods
  // ... other methods ...
}

class Edge {
  // fields
  source: Node;
  destination: Node;
  weight: Int;

  // constructor
  new(source: Node, destination: Node, weight: Int) {
    this.source = source;
    this.destination = destination;
    this.weight = weight;
  }

  // methods
  // ... other methods ...
}

// usage
var graph = new Graph([], []);

var node1 = new Node("A", 10);
var node2 = new Node("B", 20);
var node3 = new Node("C", 30);

var edge1 = new Edge(node1, node2, 5);
var edge2 = new Edge(node2, node3, 10);

graph.addNode(node1);
graph.addNode(node2);
graph.addNode(node3);

graph.addEdge(edge1);
graph.addEdge(edge2);

var nodeA = graph.findNode("A");
var nodeB = graph.findNode("B");

var edgeAB = graph.findEdge(nodeA, nodeB);

print("The weight of the edge between A and B is: ${edgeAB.weight}");
```

Explanation:

* The `Graph` class represents a graph data structure. It has a list of nodes and a list of edges.
* The `Node` class represents a node in a graph. It has a name and a value.
* The `Edge` class represents an edge in a graph. It has a source node, a destination node, and a weight.
* The `addNode` and `addEdge` methods of the `Graph` class allow you to add nodes and edges to the graph.
* The `findNode` and `findEdge` methods of the `Graph` class allow you to find a node or an edge in the graph.
* The `new` keyword is used to create new objects.
* The `.` operator is used to access the fields and methods of an object.
* The `[]` operator is used to access the elements of a list.
* The `==` operator is used to compare two values.
* The `if` statement is used to conditionally execute a block of code.
* The `for` statement is used to iterate over a list.
* The `print` statement is used to print a value to the console.

This code creates a graph with three nodes and two edges. It then finds the edge between node A and node B and prints its weight.