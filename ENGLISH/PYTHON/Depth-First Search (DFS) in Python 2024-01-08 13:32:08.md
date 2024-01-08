```python
# Program to find all paths from a source vertex to all other vertices in a graph
# using Depth-First Search (DFS) algorithm

# Class to represent a graph
class Graph:
    # Constructor
    def __init__(self, num_vertices):
        # No. of vertices
        self.V = num_vertices

        # default dictionary to store graph
        self.graph = defaultdict(list)

    # function to add an edge to graph
    def add_edge(self, u, v):
        self.graph[u].append(v)

    # A function used by DFS
    def DFSUtil(self, v, visited):
        # Mark the current node as visited
        # and print it
        visited[v] = True
        print(v, end=' ')

        # Recur for all the vertices adjacent
        # to this vertex
        for neighbour in self.graph[v]:
            if visited[neighbour] == False:
                self.DFSUtil(neighbour, visited)

    # The function to do DFS traversal. It uses
    # recursive DFSUtil()
    def DFS(self, v):
        # Create a set to store visited vertices
        visited = [False] * (self.V)

        # Call the recursive helper function
        # to print DFS traversal starting from
        # the given vertex 'v'
        self.DFSUtil(v, visited)

# Create a graph given in the above diagram
g = Graph(4)
g.add_edge(0, 1)
g.add_edge(0, 2)
g.add_edge(1, 2)
g.add_edge(2, 0)
g.add_edge(2, 3)
g.add_edge(3, 3)

print("Following is DFS from vertex 2")
g.DFS(2)

# This code is used to represent a graph and perform Depth-First Search (DFS)
# on it. The Graph class is used to create a graph, and the DFS function is used
# to perform DFS on the graph. The DFSUtil function is a recursive function that
# is used to print the vertices in DFS order.

# The time complexity of the DFS algorithm is O(V + E), where V is the number of
# vertices and E is the number of edges in the graph. This is because the DFS
# algorithm visits each vertex and edge in the graph once.

# The space complexity of the DFS algorithm is O(V), as it needs to store the
# visited vertices in a set.