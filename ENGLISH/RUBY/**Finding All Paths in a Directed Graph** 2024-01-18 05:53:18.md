```ruby

class Graph
  def initialize(nodes, edges)
    @nodes = nodes
    @edges = edges
  end

  def all_paths(source, destination)
    visited = {}
    all_paths_helper(source, destination, visited, [])
  end

  private

  def all_paths_helper(current_node, destination, visited, path)
    # Mark the current node as visited
    visited[current_node] = true
    # Add the current node to the path
    path << current_node

    # If the current node is the destination, add the path to the list of all paths
    if current_node == destination
      yield path
    # Otherwise, recursively explore all paths from the current node
    else
      @edges[current_node].each do |neighbor|
        # If the neighbor is not visited, explore it
        if !visited[neighbor]
          all_paths_helper(neighbor, destination, visited, path)
        end
      end
    end

    # Unmark the current node as visited
    visited[current_node] = false
    # Remove the current node from the path
    path.pop
  end
end

# Create a graph
nodes = ["A", "B", "C", "D", "E"]
edges = {
  "A" => ["B", "C"],
  "B" => ["D", "E"],
  "C" => ["E"],
  "D" => ["E"],
  "E" => []
}
graph = Graph.new(nodes, edges)

# Find all paths from A to E
paths = graph.all_paths("A", "E")

# Print the paths
puts "All paths from A to E:"
paths.each do |path|
  puts path.join("->")
end

```