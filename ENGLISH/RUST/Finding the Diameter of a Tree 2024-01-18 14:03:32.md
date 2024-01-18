```rust

// RUST CODE

// Importing the necessary modules
use std::io;
use std::collections::HashMap;

// Function to read and parse the input
fn read_input() -> (usize, HashMap<usize, Vec<usize>>) {
    // Reading the number of vertices and edges
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Could not read input");
    let mut parts = input.split_whitespace();
    let n_vertices: usize = parts.next().unwrap().parse().unwrap();
    let n_edges: usize = parts.next().unwrap().parse().unwrap();

    // Reading the edges and creating the adjacency list
    let mut adj_list: HashMap<usize, Vec<usize>> = HashMap::new();
    for _ in 0..n_edges {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Could not read input");
        let mut parts = input.split_whitespace();
        let v1: usize = parts.next().unwrap().parse().unwrap();
        let v2: usize = parts.next().unwrap().parse().unwrap();

        adj_list.entry(v1).or_insert(Vec::new()).push(v2);
        adj_list.entry(v2).or_insert(Vec::new()).push(v1);
    }

    (n_vertices, adj_list)
}

// Function to perform a depth-first search on the graph
fn dfs(
    u: usize,
    adj_list: &HashMap<usize, Vec<usize>>,
    visited: &mut Vec<bool>,
    parent: &mut Vec<usize>,
) {
    // Marking the current vertex as visited
    visited[u] = true;

    // Iterating over the adjacent vertices
    for &v in &adj_list[&u] {
        // If the vertex has not been visited, perform DFS on it
        if !visited[v] {
            parent[v] = u;
            dfs(v, adj_list, visited, parent);
        }
    }
}

// Function to find the diameter of the tree
fn find_diameter(
    n_vertices: usize,
    adj_list: &HashMap<usize, Vec<usize>>,
) -> (usize, Vec<usize>) {
    // Performing DFS from the first vertex to find the farthest vertex from it
    let mut visited: Vec<bool> = vec![false; n_vertices + 1];
    let mut parent: Vec<usize> = vec![0; n_vertices + 1];
    dfs(1, adj_list, &mut visited, &mut parent);

    // Finding the farthest vertex from the farthest vertex found in the previous step
    let mut farthest_vertex: usize = 0;
    for i in 1..=n_vertices {
        if visited[i] && parent[i] != 0 && parent[parent[i]] != 0 {
            farthest_vertex = i;
        }
    }

    // Performs another DFS from the farthest vertex found in the previous step to find the farthest vertex from it
    let mut visited: Vec<bool> = vec![false; n_vertices + 1];
    let mut parent: Vec<usize> = vec![0; n_vertices + 1];
    dfs(farthest_vertex, adj_list, &mut visited, &mut parent);

    // Finding the farthest vertex from the farthest vertex found in the previous step
    let mut path: Vec<usize> = Vec::new();
    let mut current_vertex = farthest_vertex;
    while current_vertex != 0 {
        path.push(current_vertex);
        current_vertex = parent[current_vertex];
    }

    (path.len() - 1, path)
}

fn main() {
    // Reading the input
    let (n_vertices, adj_list) = read_input();

    // Finding the diameter of the tree
    let (diameter, path) = find_diameter(n_vertices, &adj_list);

    // Printing the diameter and the path
    println!("{}", diameter);
    for i in 0..path.len() {
        print!("{} ", path[i]);
    }
    println!();
}

```