```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <iterator>

using namespace std;

class Graph {
public:
    Graph(int numVertices) {
        adjList.resize(numVertices);
    }

    void addEdge(int u, int v, int weight) {
        adjList[u].push_back(make_pair(v, weight));
        adjList[v].push_back(make_pair(u, weight));
    }

    void printGraph() {
        for (int i = 0; i < adjList.size(); i++) {
            cout << i << ": ";
            for (auto edge : adjList[i]) {
                cout << "(" << edge.first << ", " << edge.second << ") ";
            }
            cout << endl;
        }
    }

    int shortestPath(int start, int end) {
        vector<int> distance(adjList.size(), INT_MAX);
        distance[start] = 0;

        priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> pq;
        pq.push(make_pair(0, start));

        while (!pq.empty()) {
            int currentDistance = pq.top().first;
            int currentVertex = pq.top().second;
            pq.pop();

            if (currentDistance > distance[currentVertex]) {
                continue;
            }

            for (auto edge : adjList[currentVertex]) {
                int neighbor = edge.first;
                int weight = edge.second;

                if (distance[currentVertex] + weight < distance[neighbor]) {
                    distance[neighbor] = distance[currentVertex] + weight;
                    pq.push(make_pair(distance[neighbor], neighbor));
                }
            }
        }

        return distance[end];
    }

private:
    vector<vector<pair<int, int>>> adjList;
};

int main() {
    Graph g(9);
    g.addEdge(0, 1, 4);
    g.addEdge(0, 6, 7);
    g.addEdge(1, 2, 9);
    g.addEdge(1, 6, 14);
    g.addEdge(2, 3, 2);
    g.addEdge(2, 4, 11);
    g.addEdge(3, 4, 9);
    g.addEdge(3, 5, 6);
    g.addEdge(4, 5, 10);
    g.addEdge(5, 6, 2);
    g.addEdge(5, 7, 1);
    g.addEdge(6, 7, 15);
    g.addEdge(6, 8, 6);
    g.addEdge(7, 8, 1);

    cout << "Graph:" << endl;
    g.printGraph();

    cout << "Shortest path from vertex 0 to vertex 8: " << g.shortestPath(0, 8) << endl;

    return 0;
}
```

This code implements a graph data structure in C++ and uses Dijkstra's algorithm to find the shortest path between two vertices in the graph.

The `Graph` class represents the graph, and it has methods to add edges, print the graph, and find the shortest path between two vertices.

The `addEdge` method adds an edge to the graph, and the `printGraph` method prints the graph in a human-readable format.

The `shortestPath` method uses Dijkstra's algorithm to find the shortest path between two vertices in the graph. Dijkstra's algorithm works by maintaining a priority queue of vertices, where the priority of a vertex is its distance from the starting vertex. The algorithm starts by setting the distance of the starting vertex to 0 and the distance of all other vertices to infinity. It then repeatedly dequeues the vertex with the smallest distance from the priority queue and updates the distances of its neighbors. This process continues until the algorithm has found the shortest path to all vertices in the graph.

The `main` function creates a graph and adds some edges to it. It then prints the graph and finds the shortest path between two vertices in the graph.