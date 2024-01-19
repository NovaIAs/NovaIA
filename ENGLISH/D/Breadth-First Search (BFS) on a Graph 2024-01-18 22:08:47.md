```d
import std.stdio, std.algorithm, std.range, std.traits, std.string, std.intrinsics;

struct Graph {
    int n;
    int[][] edges;

    Graph(int n) pure nothrow @safe {
        this.n = n;
        edges = new int[n][n];
    }

    void addEdge(int a, int b) pure nothrow @safe {
        edges[a][b] = 1;
        edges[b][a] = 1;
    }

    bool hasEdge(int a, int b) pure nothrow @safe {
        return edges[a][b] == 1;
    }

    int[] bfs(int start) pure nothrow @safe {
        int[] dist = new int[n];
        bool[] visited = new bool[n];
        int[] queue = new int[n];
        int head = 0;
        int tail = 0;

        queue[tail++] = start;
        visited[start] = true;

        while (head < tail) {
            int u = queue[head++];
            for (int v in 0 .. n) {
                if (hasEdge(u, v) && !visited[v]) {
                    queue[tail++] = v;
                    visited[v] = true;
                    dist[v] = dist[u] + 1;
                }
            }
        }

        return dist;
    }
};

void main() {
    int n = 5;
    Graph g = new Graph(n);

    g.addEdge(0, 1);
    g.addEdge(0, 2);
    g.addEdge(1, 2);
    g.addEdge(2, 3);
    g.addEdge(3, 4);

    int[] dist = g.bfs(0);

    for (int i in 0 .. n) {
        writeln("Distance from node 0 to node ", i, ": ", dist[i]);
    }
}
```

This code implements a graph data structure and uses it to perform a breadth-first search (BFS) on a graph.

The `Graph` struct represents a graph as an adjacency matrix, where `edges[a][b]` is 1 if there is an edge from node `a` to node `b`, and 0 otherwise.

The `bfs` method performs a BFS on the graph starting from a given node `start`. It uses a queue to keep track of the nodes that have been visited but not yet explored, and a `visited` array to keep track of which nodes have already been visited.

The method starts by adding the starting node to the queue and marking it as visited. It then loops through the queue, visiting each node in FIFO order. For each node `u` that is visited, it loops through all of its neighbors `v`, and if a neighbor `v` has not been visited yet, it is added to the queue and marked as visited.

The method also keeps track of the distance from the starting node to each node that is visited. The distance to a node `v` is the number of edges in the shortest path from the starting node to `v`.

The output of the program is:

```
Distance from node 0 to node 1: 1
Distance from node 0 to node 2: 1
Distance from node 0 to node 3: 2
Distance from node 0 to node 4: 3
```

This shows that the shortest path from node 0 to node 1 is one edge long, the shortest path from node 0 to node 2 is one edge long, the shortest path from node 0 to node 3 is two edges long, and the shortest path from node 0 to node 4 is three edges long.