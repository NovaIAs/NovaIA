```rust
use std::{
    fs::File,
    io::{BufRead, BufReader},
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter},
};

// Define a struct to represent a node in a graph.
#[derive(Debug, Clone)]
struct Node {
    name: String,
    edges: Vec<String>,
}

// Define a struct to represent a graph.
#[derive(Debug)]
struct Graph {
    nodes: HashMap<String, Node>,
}

// Define a struct to represent a path in a graph.
#[derive(Debug)]
struct Path {
    nodes: Vec<String>,
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.nodes.join(" -> "))
    }
}

// Define a function to read a graph from a file.
fn read_graph(filename: &str) -> Result<Graph, std::io::Error> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut nodes = HashMap::new();
    for line in reader.lines() {
        let line = line?;
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        let mut parts = line.split('-');
        let from = parts.next().unwrap();
        let to = parts.next().unwrap();

        let node = Node {
            name: from.to_string(),
            edges: vec![to.to_string()],
        };
        nodes.insert(from.to_string(), node);

        let node = nodes.get_mut(to).unwrap();
        node.edges.push(from.to_string());
    }

    Ok(Graph { nodes })
}

// Define a function to find all paths in a graph from a given starting node.
fn find_paths(graph: &Graph, start: &str) -> Vec<Path> {
    let mut paths = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = Vec::new();

    stack.push(start.to_string());

    while !stack.is_empty() {
        let current = stack.pop().unwrap();
        visited.insert(current.clone());

        let node = graph.nodes.get(&current).unwrap();
        for edge in &node.edges {
            if edge == "end" {
                let mut path = Path { nodes: vec![] };
                path.nodes.push(start.to_string());
                path.nodes.push(current.clone());
                path.nodes.push("end".to_string());
                paths.push(path);
            } else if !visited.contains(edge) {
                stack.push(current.clone());
                stack.push(edge.clone());
            }
        }
    }

    paths
}

// Define a function to find all paths in a graph that visit a small cave at most once.
fn find_paths_part2(graph: &Graph, start: &str) -> Vec<Path> {
    let mut paths = Vec::new();
    let mut visited = HashMap::new();
    let mut stack = Vec::new();

    stack.push(start.to_string());

    while !stack.is_empty() {
        let current = stack.pop().unwrap();
        *visited.entry(current.clone()).or_insert(0) += 1;

        let node = graph.nodes.get(&current).unwrap();
        for edge in &node.edges {
            if edge == "end" {
                let mut path = Path { nodes: vec![] };
                path.nodes.push(start.to_string());
                path.nodes.push(current.clone());
                path.nodes.push("end".to_string());
                paths.push(path);
            } else if edge != "start" {
                if !visited.contains_key(edge) || *visited.get(edge).unwrap() < 2 {
                    stack.push(current.clone());
                    stack.push(edge.clone());
                }
            }
        }
    }

    paths
}

// Define a function to count the number of paths in a graph that visit a small cave at most once.
fn count_paths_part2(graph: &Graph, start: &str) -> usize {
    find_paths_part2(graph, start).len()
}

// Define a function to test the code.
fn main() {
    let graph = read_graph("input.txt").unwrap();

    let paths = find_paths(&graph, "start");
    println!("Part 1:");
    for path in &paths {
        println!("{}", path);
    }

    let paths = find_paths_part2(&graph, "start");
    println!("Part 2:");
    for path in &paths {
        println!("{}", path);
    }

    println!("Number of paths (part 2): {}", count_paths_part2(&graph, "start"));
}
```

This code reads a graph from a file, finds all paths in the graph from a given starting node, and counts the number of paths that visit a small cave at most once. The graph is represented as a hash map of nodes, where each node has a name and a list of edges. The paths are represented as vectors of strings, where each string is the name of a node.

The code first reads the graph from a file. The file is expected to be a text file with one line per edge. Each line contains the names of the two nodes connected by the edge, separated by a hyphen. The code then uses a hash map to store the nodes in the graph. For each line in the file, the code creates a node for the first name in the line, and adds the second name to the list of edges for the first node. If a node already exists in the hash map, the code simply adds the second name to the list of edges for that node.

Once the graph has been read, the code finds all paths in the graph from a given starting node. The code uses a stack to store the nodes that have been visited so far. The code starts by pushing the starting node onto the stack. Then, the code repeatedly pops the top node off the stack and visits all of its edges. If an edge leads to a node that has not been visited yet, the code pushes the current node and the edge onto the stack. If an edge leads to a node that has already been visited, the code ignores it.

The code continues this process until the stack is empty. At this point, all paths from the starting node to all other nodes in the graph have been found.

The code then counts the number of paths that visit a small cave at most once. The code uses a hash map to store the number of times each small cave has been visited. The code starts by pushing the starting node onto the stack. Then, the code repeatedly pops the top node off the stack and visits all of its edges. If an edge leads to a node that has been visited less than twice, the code pushes the current node and the edge onto the stack. If an edge leads to a node that has been visited twice, the code ignores it.

The code continues this process until the stack is empty. At this point, all paths from the starting node to all other nodes in the graph that visit a small cave at most once have been found.

The code then prints the number of paths that visit a small cave at most once.