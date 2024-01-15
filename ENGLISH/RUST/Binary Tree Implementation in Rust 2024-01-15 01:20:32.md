```rust
// Define a struct to represent a node in a binary tree
#[derive(Debug)]
struct Node<T> {
    // The data stored in the node
    data: T,
    // The left child node
    left: Option<Box<Node<T>>>,
    // The right child node
    right: Option<Box<Node<T>>>,
}

// Implement the Node struct
impl<T> Node<T> {
    // Create a new node with the given data
    fn new(data: T) -> Self {
        Node {
            data,
            left: None,
            right: None,
        }
    }

    // Insert a new node into the tree
    fn insert(&mut self, data: T) {
        // If the data is less than the current node's data, insert it into the left subtree
        if data < self.data {
            match self.left {
                // If the left subtree is empty, create a new node and insert it
                None => {
                    self.left = Some(Box::new(Node::new(data)));
                }
                // Otherwise, recurse down the left subtree
                Some(ref mut node) => {
                    node.insert(data);
                }
            }
        }
        // Otherwise, insert it into the right subtree
        else {
            match self.right {
                // If the right subtree is empty, create a new node and insert it
                None => {
                    self.right = Some(Box::new(Node::new(data)));
                }
                // Otherwise, recurse down the right subtree
                Some(ref mut node) => {
                    node.insert(data);
                }
            }
        }
    }

    // Search for a node with the given data in the tree
    fn search(&self, data: T) -> Option<&Node<T>> {
        // If the data is equal to the current node's data, return the current node
        if data == self.data {
            return Some(self);
        }
        // If the data is less than the current node's data, search the left subtree
        else if data < self.data {
            return self.left.as_ref().and_then(|node| node.search(data));
        }
        // Otherwise, search the right subtree
        else {
            return self.right.as_ref().and_then(|node| node.search(data));
        }
    }

    // Delete a node with the given data from the tree
    fn delete(&mut self, data: T) {
        // If the data is less than the current node's data, delete it from the left subtree
        if data < self.data {
            match self.left {
                // If the left subtree is empty, there is nothing to delete
                None => {}
                // Otherwise, recurse down the left subtree
                Some(ref mut node) => {
                    node.delete(data);
                }
            }
        }
        // Otherwise, delete it from the right subtree
        else if data > self.data {
            match self.right {
                // If the right subtree is empty, there is nothing to delete
                None => {}
                // Otherwise, recurse down the right subtree
                Some(ref mut node) => {
                    node.delete(data);
                }
            }
        }
        // If the data is equal to the current node's data, delete the current node
        else {
            // If the current node has no children, simply delete it
            if self.left.is_none() && self.right.is_none() {
                *self = Node::new(std::mem::replace(&mut self.data, data));
            }
            // If the current node has only a left child, replace the current node with the left child
            else if self.left.is_some() && self.right.is_none() {
                *self = *self.left.as_mut().unwrap();
            }
            // If the current node has only a right child, replace the current node with the right child
            else if self.left.is_none() && self.right.is_some() {
                *self = *self.right.as_mut().unwrap();
            }
            // Otherwise, find the smallest node in the right subtree and replace the current node with that node
            else {
                let mut min_node = self.right.as_mut().unwrap();
                while min_node.left.is_some() {
                    min_node = min_node.left.as_mut().unwrap();
                }
                self.data = min_node.data;
                min_node.delete(min_node.data);
            }
        }
    }

    // Print the tree in pre-order traversal
    fn print_preorder(&self) {
        print!("{} ", self.data);
        if let Some(ref left) = self.left {
            left.print_preorder();
        }
        if let Some(ref right) = self.right {
            right.print_preorder();
        }
    }

    // Print the tree in in-order traversal
    fn print_inorder(&self) {
        if let Some(ref left) = self.left {
            left.print_inorder();
        }
        print!("{} ", self.data);
        if let Some(ref right) = self.right {
            right.print_inorder();
        }
    }

    // Print the tree in post-order traversal
    fn print_postorder(&self) {
        if let Some(ref left) = self.left {
            left.print_postorder();
        }
        if let Some(ref right) = self