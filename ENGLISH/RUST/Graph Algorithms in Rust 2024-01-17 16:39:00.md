```
// Import necessary libraries
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

// Define a struct to represent a graph
#[derive(Debug)]
struct Graph {
    nodes: HashMap<String, usize>,
    edges: Vec<(usize, usize)>,
}

// Implement the Graph struct
impl Graph {
    // Create a new graph
    fn new() -> Graph {
        Graph {
            nodes: HashMap::new(),
            edges: Vec::new(),
        }
    }

    // Add a node to the graph
    fn add_node(&mut self, node: &str) {
        if !self.nodes.contains_key(node) {
            let index = self.nodes.len();
            self.nodes.insert(node.to_string(), index);
        }
    }

    // Add an edge to the graph
    fn add_edge(&mut self, from: &str, to: &str) {
        let from_index = self.nodes[from];
        let to_index = self.nodes[to];
        self.edges.push((from_index, to_index));
    }

    // Generate a random graph
    fn generate_random_graph(num_nodes: usize, num_edges: usize) -> Graph {
        let mut graph = Graph::new();

        // Add nodes to the graph
        for i in 0..num_nodes {
            graph.add_node(&format!("Node{}", i));
        }

        // Add edges to the graph
        for _ in 0..num_edges {
            let from = thread_rng().gen_range(0, num_nodes);
            let to = thread_rng().gen_range(0, num_nodes);
            graph.add_edge(&format!("Node{}", from), &format!("Node{}", to));
        }

        graph
    }

    // Load a graph from a file
    fn load_from_file(filename: &str) -> Graph {
        let mut graph = Graph::new();

        let file = File::open(filename).expect("Could not open file");
        let reader = BufReader::new(file);

        for line in reader.lines() {
            let line = line.expect("Could not read line");
            let mut parts = line.split_whitespace();

            let from = parts.next().expect("Could not get from node");
            let to = parts.next().expect("Could not get to node");

            graph.add_node(from);
            graph.add_node(to);
            graph.add_edge(from, to);
        }

        graph
    }

    // Depth-first search
    fn depth_first_search(&self, start: &str) {
        let start_index = self.nodes[start];

        let mut visited = vec![false; self.nodes.len()];
        visited[start_index] = true;

        let mut stack = vec![start_index];

        while !stack.is_empty() {
            let current = stack.pop().unwrap();

            println!("Visiting node: {}", start);

            for edge in &self.edges {
                if edge.0 == current && !visited[edge.1] {
                    visited[edge.1] = true;
                    stack.push(edge.1);
                }
            }
        }
    }

    // Breadth-first search
    fn breadth_first_search(&self, start: &str) {
        let start_index = self.nodes[start];

        let mut visited = vec![false; self.nodes.len()];
        visited[start_index] = true;

        let mut queue = vec![start_index];

        while !queue.is_empty() {
            let current = queue.remove(0);

            println!("Visiting node: {}", start);

            for edge in &self.edges {
                if edge.0 == current && !visited[edge.1] {
                    visited[edge.1] = true;
                    queue.push(edge.1);
                }
            }
        }
    }

    // Dijkstra's algorithm
    fn dijkstras_algorithm(&self, start: &str) {
        let start_index = self.nodes[start];

        let mut distances = vec![usize::MAX; self.nodes.len()];
        distances[start_index] = 0;

        let mut visited = vec![false; self.nodes.len()];

        while !visited.iter().all(|&x| x) {
            // Find the unvisited node with the smallest distance
            let mut min_distance = usize::MAX;
            let mut min_index = 0;
            for i in 0..self.nodes.len() {
                if !visited[i] && distances[i] < min_distance {
                    min_distance = distances[i];
                    min_index = i;
                }
            }

            // Visit the node
            visited[min_index] = true;

            // Update the distances of its neighbors
            for edge in &self.edges {
                if edge.0 == min_index {
                    let