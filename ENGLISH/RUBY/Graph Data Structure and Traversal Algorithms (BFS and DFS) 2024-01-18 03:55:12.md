```ruby
# Class definition of a Graph data structure
class Graph:

    # Constructor
    def initialize()
        # Initialize the adjacency list
        @adj_list = {}
    end

    # Method to add a node to the graph
    def add_node(node):
        # If the node is not already in the adjacency list, add it
        if node not in @adj_list:
            @adj_list[node] = []

    # Method to add an edge between two nodes
    def add_edge(src, dest):
        # Add the destination node to the adjacency list of the source node
        @adj_list[src].append(dest)

    # Method to perform breadth-first search (BFS) starting from a given node
    def bfs(start_node):
        # Initialize a queue for BFS
        queue = []

        # Initialize a set to keep track of visited nodes
        visited = set()

        # Add the starting node to the queue and mark it as visited
        queue.append(start_node)
        visited.add(start_node)

        # While the queue is not empty
        while queue:

            # Dequeue the first node from the queue
            current_node = queue.pop(0)

            # Print the current node
            print(current_node)

            # Enqueue all neighbors of the current node that have not been visited yet
            for neighbor in @adj_list[current_node]:
                if neighbor not in visited:
                    queue.append(neighbor)
                    visited.add(neighbor)

    # Method to perform depth-first search (DFS) starting from a given node
    def dfs(start_node):
        # Initialize a stack for DFS
        stack = []

        # Initialize a set to keep track of visited nodes
        visited = set()

        # Push the starting node to the stack and mark it as visited
        stack.append(start_node)
        visited.add(start_node)

        # While the stack is not empty
        while stack:

            # Pop the top element from the stack
            current_node = stack.pop()

            # Print the current node
            print(current_node)

            # Push all unvisited neighbors of the current node to the stack
            for neighbor in @adj_list[current_node]:
                if neighbor not in visited:
                    stack.append(neighbor)
                    visited.add(neighbor)

# Creating a graph object
graph = Graph()

# Adding nodes to the graph
graph.add_node('A')
graph.add_node('B')
graph.add_node('C')
graph.add_node('D')
graph.add_node('E')

# Adding edges to the graph
graph.add_edge('A', 'B')
graph.add_edge('A', 'C')
graph.add_edge('B', 'D')
graph.add_edge('C', 'E')
graph.add_edge('D', 'E')

# Performing BFS starting from node 'A'
print("BFS:")
graph.bfs('A')

# Performing DFS starting from node 'A'
print("DFS:")
graph.dfs('A')
```

Explanation:

1. We define a Graph class that represents the graph data structure.

2. The Graph class has an initialize method that initializes an adjacency list, which is a dictionary where keys are nodes and values are lists of their adjacent nodes.

3. The add_node method adds a node to the graph if it doesn't already exist.

4. The add_edge method adds a directed edge from one node to another by appending the destination node to the adjacency list of the source node.

5. The bfs method performs a breadth-first search starting from a given node. It uses a queue to keep track of nodes that need to be visited. It starts by adding the starting node to the queue and marking it as visited. Then, it dequeues nodes from the queue and enqueues their unvisited neighbors until the queue is empty.

6. The dfs method performs a depth-first search starting from a given node. It uses a stack to keep track of nodes that need to be visited. It starts by pushing the starting node to the stack and marking it as visited. Then, it pops nodes from the stack and pushes their unvisited neighbors to the stack until the stack is empty.

7. We create a Graph object and add nodes and edges to it.

8. We perform BFS and DFS starting from node 'A' and print the visited nodes in each case.