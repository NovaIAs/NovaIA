```rust
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display},
};

#[derive(Debug)]
struct Node {
    name: String,
    children: Vec<Node>,
}

impl Node {
    fn new(name: &str) -> Self {
        Node {
            name: name.to_string(),
            children: Vec::new(),
        }
    }

    fn add_child(&mut self, child: Node) {
        self.children.push(child);
    }

    fn find_node(&self, name: &str) -> Option<&Node> {
        if self.name == name {
            return Some(self);
        }

        for child in &self.children {
            if let Some(node) = child.find_node(name) {
                return Some(node);
            }
        }

        None
    }

    fn calculate_height(&self) -> usize {
        let mut max_height = 0;
        for child in &self.children {
            let height = child.calculate_height();
            if height > max_height {
                max_height = height;
            }
        }

        max_height + 1
    }

    fn calculate_diameter(&self) -> usize {
        let mut max_diameter = 0;
        for child in &self.children {
            let diameter = child.calculate_diameter();
            if diameter > max_diameter {
                max_diameter = diameter;
            }

            let height = child.calculate_height();
            let diameter_through_child = height + self.calculate_height();
            if diameter_through_child > max_diameter {
                max_diameter = diameter_through_child;
            }
        }

        max_diameter
    }

    fn find_ancestors(&self, name: &str) -> Option<Vec<&Node>> {
        if self.name == name {
            return Some(vec![]);
        }

        for child in &self.children {
            if let Some(mut ancestors) = child.find_ancestors(name) {
                ancestors.insert(0, self);
                return Some(ancestors);
            }
        }

        None
    }

    fn find_common_ancestor(&self, name1: &str, name2: &str) -> Option<&Node> {
        let ancestors1 = self.find_ancestors(name1)?;
        let ancestors2 = self.find_ancestors(name2)?;

        for i in 0..ancestors1.len() {
            for j in 0..ancestors2.len() {
                if ancestors1[i] == ancestors2[j] {
                    return Some(ancestors1[i]);
                }
            }
        }

        None
    }

    fn calculate_path_length(&self, name1: &str, name2: &str) -> Option<usize> {
        if let Some(common_ancestor) = self.find_common_ancestor(name1, name2) {
            let path_length1 = common_ancestor
                .find_ancestors(name1)
                .unwrap()
                .len();
            let path_length2 = common_ancestor
                .find_ancestors(name2)
                .unwrap()
                .len();

            return Some(path_length1 + path_length2 - 2);
        }

        None
    }

    fn find_lowest_common_ancestor(&self, name1: &str, name2: &str) -> Option<&Node> {
        let ancestors1 = self.find_ancestors(name1)?;
        let ancestors2 = self.find_ancestors(name2)?;

        let mut lca = None;
        for i in 0..ancestors1.len() {
            for j in 0..ancestors2.len() {
                if ancestors1[i] == ancestors2[j] {
                    lca = Some(ancestors1[i]);
                    break;
                }
            }
        }

        lca
    }

    fn is_subtree(&self, other: &Node) -> bool {
        self.has_subtree(other)
    }

    fn has_subtree(&self, other: &Node) -> bool {
        if self.name == other.name {
            return true;
        }

        for child in &self.children {
            if child.has_subtree(other) {
                return true;
            }
        }

        false
    }

    fn calculate_size(&self) -> usize {
        let mut size = 1;
        for child in &self.children {
            size += child.calculate_size();
        }

        size
    }

    fn find_kth_largest_node(&self, k: usize) -> Option<&Node> {
        let mut nodes: Vec<&Node> = Vec::new();
        self.inorder_traversal(&mut nodes);

        nodes.sort_by_key(|node| node.name);

        if k <= nodes.len() {
            return Some(nodes[k - 1]);
        }

        None
    }

    fn inorder_traversal(&self, nodes: &mut Vec<&Node>) {
        nodes.push(self);
        for child in &self.children {
            child.inorder_traversal(nodes);
        }
    }

    fn find_kth_smallest_node(&self, k: usize) -> Option<&Node> {
        let mut nodes: Vec<&Node> = Vec::new();
        self.preorder_traversal(&mut nodes);

        nodes.sort_by_key(|node| node.name);

        if k <= nodes.len() {
            return Some(nodes[k - 1]);
        }

        None
    }

    fn preorder_traversal(&self, nodes: &mut Vec<&Node>) {
        for child in &self.children