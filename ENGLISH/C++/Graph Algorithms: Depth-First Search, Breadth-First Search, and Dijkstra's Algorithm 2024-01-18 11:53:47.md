```c++
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>
#include <unordered_map>
#include <random>

using namespace std;

// Define a custom data structure for representing a graph
struct Edge {
    int source;
    int destination;
    int weight;
};

struct Graph {
    vector<vector<Edge>> adjacency_list;
    unordered_map<string, int> vertex_names_to_ids;
};

// Function to add a vertex to the graph
void add_vertex(Graph& graph, string vertex_name) {
    int vertex_id = graph.vertex_names_to_ids.size();
    graph.vertex_names_to_ids[vertex_name] = vertex_id;
    graph.adjacency_list.push_back(vector<Edge>());
}

// Function to add an edge to the graph
void add_edge(Graph& graph, string vertex_name1, string vertex_name2, int weight) {
    int vertex_id1 = graph.vertex_names_to_ids[vertex_name1];
    int vertex_id2 = graph.vertex_names_to_ids[vertex_name2];
    graph.adjacency_list[vertex_id1].push_back({vertex_id1, vertex_id2, weight});
    graph.adjacency_list[vertex_id2].push_back({vertex_id2, vertex_id1, weight});
}

// Function to perform a depth-first search on the graph
vector<int> depth_first_search(Graph& graph, int start_vertex) {
    vector<int> visited_vertices;
    vector<bool> visited(graph.adjacency_list.size(), false);
    stack<int> stack;

    stack.push(start_vertex);
    visited[start_vertex] = true;

    while (!stack.empty()) {
        int current_vertex = stack.top();
        stack.pop();
        visited_vertices.push_back(current_vertex);

        for (Edge edge : graph.adjacency_list[current_vertex]) {
            int adjacent_vertex = edge.destination;
            if (!visited[adjacent_vertex]) {
                stack.push(adjacent_vertex);
                visited[adjacent_vertex] = true;
            }
        }
    }

    return visited_vertices;
}

// Function to perform a breadth-first search on the graph
vector<int> breadth_first_search(Graph& graph, int start_vertex) {
    vector<int> visited_vertices;
    vector<bool> visited(graph.adjacency_list.size(), false);
    queue<int> queue;

    queue.push(start_vertex);
    visited[start_vertex] = true;

    while (!queue.empty()) {
        int current_vertex = queue.front();
        queue.pop();
        visited_vertices.push_back(current_vertex);

        for (Edge edge : graph.adjacency_list[current_vertex]) {
            int adjacent_vertex = edge.destination;
            if (!visited[adjacent_vertex]) {
                queue.push(adjacent_vertex);
                visited[adjacent_vertex] = true;
            }
        }
    }

    return visited_vertices;
}

// Function to find the shortest path between two vertices in the graph using Dijkstra's algorithm
vector<int> dijkstra(Graph& graph, int start_vertex, int end_vertex) {
    // Initialize distances to infinity for all vertices
    vector<int> distances(graph.adjacency_list.size(), numeric_limits<int>::max());
    distances[start_vertex] = 0;

    // Initialize a priority queue to store vertices based on their distances
    priority_queue<tuple<int, int>, vector<tuple<int, int>>, greater<tuple<int, int>>> pq;
    pq.push(make_tuple(0, start_vertex));

    // While the priority queue is not empty
    while (!pq.empty()) {
        // Get the vertex with the smallest distance
        int current_vertex = get<1>(pq.top());
        pq.pop();

        // If the current vertex is the end vertex, we have found the shortest path
        if (current_vertex == end_vertex) {
            break;
        }

        // Relax all adjacent vertices
        for (Edge edge : graph.adjacency_list[current_vertex]) {
            int adjacent_vertex = edge.destination;
            int weight = edge.weight;

            // If the distance to the adjacent vertex can be improved, update the distance and the priority queue
            if (distances[current_vertex] + weight < distances[adjacent_vertex]) {
                distances[adjacent_vertex] = distances[current_vertex] + weight;
                pq.push(make_tuple(distances[adjacent_vertex], adjacent_vertex));
            }
        }
    }

    // Reconstruct the shortest path
    vector<int> shortest_path;
    int current_vertex = end_vertex;
    while (current_vertex != start_vertex) {
        for (Edge edge : graph.adjacency_list[current_vertex]) {
            int adjacent_vertex = edge.destination;
            int weight = edge.weight;

            if (distances[current_vertex] - weight == distances[adjacent_vertex]) {
                shortest_path.push_back(current_vertex);
                current_vertex = adjacent_vertex;
                break;
            }
        }
    }
    shortest_path.push_back(start_vertex);

    // Reverse the shortest path to get the correct order
    reverse(shortest_path.begin(), shortest_path.end());

    return shortest_path;
}

// Function to generate a random graph
Graph generate_random_graph(int num_vertices, int num_edges) {
    Graph graph;

    // Add vertices to the graph
    for (int i = 0; i < num_vertices; i++) {
        add_vertex(graph, "Vertex" + to_string(i));
    }

    // Add edges to the graph
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<int> dist(0, num_vertices - 1);

    for (int i = 0; i < num_edges; i++) {
        int vertex_id1 = dist(gen);
        int vertex_id2 = dist(gen);
        int weight = rand() % 10 + 1;

        add_edge(graph, "Vertex" + to_string(vertex_id1), "Vertex" + to_string(vertex_id2), weight);
    }

    return graph;
}

int main() {
    // Create a graph
    Graph graph = generate_random_graph(10, 20);

    // Perform depth-first search on the graph
    vector<int> visited_vertices_dfs = depth_first_search(graph, 0);
    cout << "Depth-first search: ";
    for (int vertex_id : visited_vertices_dfs) {
        cout << "Vertex" + to_string(vertex_id) << " ";
    }
    cout << endl;

    // Perform breadth-first search on the graph
    vector<int> visited_vertices_bfs = breadth_first_search(graph, 0);
    cout << "Breadth-first search: ";
    for (int vertex_id : visited_vertices_bfs) {
        cout << "Vertex" + to_string(vertex_id) << " ";
    }
    cout << endl;

    // Find the shortest path between two vertices using Dijkstra's algorithm
    vector<int> shortest_path = dijkstra(graph