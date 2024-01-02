Certainly! Here's a complex and unique Ruby code that performs a network analysis. It uses a combination of graph theory and breadth-first search algorithm to find the shortest path between two nodes in a directed graph. Let me explain the code step by step:

```ruby
require 'set'

class Node
  attr_reader :name, :neighbors

  def initialize(name)
    @name = name
    @neighbors = {}
  end

  def add_neighbor(node, weight)
    @neighbors[node] = weight
  end
end

class Graph
  def initialize
    @nodes = {}
  end

  def add_node(name)
    @nodes[name] = Node.new(name)
  end

  def add_edge(from, to, weight)
    @nodes[from].add_neighbor(@nodes[to], weight)
  end

  def shortest_path(start, target)
    distance = {}
    previous = {}
    unvisited = Set.new

    @nodes.each_key do |node_name|
      distance[node_name] = Float::INFINITY
      previous[node_name] = nil
      unvisited << node_name
    end

    distance[start] = 0

    while !unvisited.empty?
      current = unvisited.min_by { |node_name| distance[node_name] }
      unvisited.delete(current)

      break if current == target

      @nodes[current].neighbors.each do |neighbor, weight|
        alt = distance[current] + weight
        if alt < distance[neighbor.name]
          distance[neighbor.name] = alt
          previous[neighbor.name] = current
        end
      end
    end

    path = []
    node = target

    while node != nil
      path.unshift(node)
      node = previous[node]
    end

    path
  end
end

# Usage example

graph = Graph.new

graph.add_node('A')
graph.add_node('B')
graph.add_node('C')
graph.add_node('D')
graph.add_node('E')
graph.add_node('F')

graph.add_edge('A', 'B', 3)
graph.add_edge('A', 'C', 4)
graph.add_edge('B', 'C', 1)
graph.add_edge('B', 'D', 2)
graph.add_edge('C', 'D', 5)
graph.add_edge('D', 'E', 7)
graph.add_edge('E', 'F', 2)
graph.add_edge('D', 'F', 4)

start_node = 'A'
target_node = 'F'

shortest_path = graph.shortest_path(start_node, target_node)

puts "Shortest path from #{start_node} to #{target_node}: #{shortest_path.join(' -> ')}"
```

Explanation:
The code begins by defining two classes, `Node` and `Graph`. The `Node` class represents a single node in the graph, with a name and a hash to store its neighbors and corresponding edge weights. The `Graph` class represents the entire directed graph, storing the nodes in a hash.

The `Graph` class has methods to add nodes and edges to the graph. The `add_node` method creates a new instance of `Node` and adds it to the graph. The `add_edge` method connects two nodes by adding a neighbor and its corresponding weight to the source node's neighbor hash.

The `shortest_path` method in the `Graph` class finds the shortest path between two nodes using the breadth-first search algorithm. It initializes distance and previous hash tables to track the minimum distance from the start node to each node and the previous node on the shortest path. It also initializes a `Set` to keep track of unvisited nodes.

The algorithm iterates until all nodes are visited or until the target node is reached. In each iteration, it selects the node with the minimum distance from the `unvisited` set and removes it. Then, it updates the minimum distance to its neighbors if a shorter path is found.

After the algorithm finishes, the shortest path is reconstructed by following the `previous` hash table from the target node to the start node. The path is then printed to the console.

In the usage example, a graph is created with six nodes (A, B, C, D, E, and F) and several edges connecting them. The shortest path between node A and F is then computed using the `shortest_path` method and printed to the console.