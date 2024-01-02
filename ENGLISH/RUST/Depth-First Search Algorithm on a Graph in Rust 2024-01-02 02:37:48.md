Certainly! Here's a complex and unique code written in Rust:

```rust
use std::collections::HashMap;

struct Graph {
    vertices: HashMap<u32, Vec<u32>>,
}

impl Graph {
    fn new() -> Self {
        Graph {
            vertices: HashMap::new(),
        }
    }

    fn add_vertex(&mut self, vertex: u32) {
        self.vertices.insert(vertex, vec![]);
    }

    fn add_edge(&mut self, source: u32, destination: u32) {
        self.vertices.entry(source).or_insert_with(Vec::new).push(destination);
        self.vertices.entry(destination).or_insert_with(Vec::new);
    }

    fn get_neighbors(&self, vertex: u32) -> Option<&Vec<u32>> {
        self.vertices.get(&vertex)
    }
}

fn depth_first_search(graph: &Graph, start_vertex: u32) -> Vec<u32> {
    let mut visited: HashMap<u32, bool> = HashMap::new();
    let mut result: Vec<u32> = Vec::new();

    dfs_recursive(graph, start_vertex, &mut visited, &mut result);

    result
}

fn dfs_recursive(graph: &Graph, vertex: u32, visited: &mut HashMap<u32, bool>, result: &mut Vec<u32>) {
    visited.insert(vertex, true);
    result.push(vertex);

    if let Some(neighbors) = graph.get_neighbors(vertex) {
        for &neighbor in neighbors {
            if !visited.contains_key(&neighbor) {
                dfs_recursive(graph, neighbor, visited, result);
            }
        }
    }
}

fn main() {
    let mut graph = Graph::new();

    graph.add_vertex(1);
    graph.add_vertex(2);
    graph.add_vertex(3);
    graph.add_vertex(4);

    graph.add_edge(1, 2);
    graph.add_edge(2, 3);
    graph.add_edge(3, 4);
    graph.add_edge(4, 1);

    let result = depth_first_search(&graph, 1);
    println!("Depth-First Search Result: {:?}", result);
}
```

This code implements a depth-first search algorithm on a graph data structure using Rust. It defines a `Graph` struct with methods to add vertices and edges, as well as retrieve neighbors. The `depth_first_search` function performs a depth-first search traversal starting from a specified vertex. The `dfs_recursive` function recursively explores the graph, marking visited vertices and appending them to the result vector.

In the `main` function, a new `Graph` is created, vertices and edges are added, and the depth-first search is performed starting from vertex 1. The resulting traversal is printed to the console.

The code utilizes Rust's HashMap to efficiently represent the graph's adjacency list. It demonstrates some key concepts in Rust, such as ownership, borrowing, and recursion.