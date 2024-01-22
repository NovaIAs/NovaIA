```rust
// Importar las bibliotecas necesarias.
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

// Definir una estructura para representar un nodo en un árbol binario.
#[derive(Debug)]
struct Node {
    value: i32,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

// Definir una función para crear un nuevo nodo en un árbol binario.
fn create_node(value: i32) -> Box<Node> {
    Box::new(Node {
        value,
        left: None,
        right: None,
    })
}

// Definir una función para insertar un nuevo nodo en un árbol binario.
fn insert_node(root: &mut Box<Node>, value: i32) {
    if value < root.value {
        if let Some(left) = &mut root.left {
            insert_node(left, value);
        } else {
            root.left = Some(create_node(value));
        }
    } else {
        if let Some(right) = &mut root.right {
            insert_node(right, value);
        } else {
            root.right = Some(create_node(value));
        }
    }
}

// Definir una función para buscar un nodo en un árbol binario.
fn search_node(root: &Box<Node>, value: i32) -> Option<&Box<Node>> {
    if value == root.value {
        Some(root)
    } else if value < root.value {
        if let Some(left) = &root.left {
            search_node(left, value)
        } else {
            None
        }
    } else {
        if let Some(right) = &root.right {
            search_node(right, value)
        } else {
            None
        }
    }
}

// Definir una función para eliminar un nodo de un árbol binario.
fn delete_node(root: &mut Box<Node>, value: i32) {
    if let Some(left) = &mut root.left {
        if left.value == value {
            if left.left.is_none() {
                root.left = left.right.take();
            } else if let Some(right) = &mut left.right {
                *left = **right;
                delete_node(right, value);
            }
        } else {
            delete_node(left, value);
        }
    } else if let Some(right) = &mut root.right {
        if right.value == value {
            if right.left.is_none() {
                root.right = right.right.take();
            } else if let Some(left) = &mut right.left {
                *right = **left;
                delete_node(left, value);
            }
        } else {
            delete_node(right, value);
        }
    }
}

// Definir una función para imprimir un árbol binario en preorden.
fn print_preorder(root: &Box<Node>) {
    print!("{} ", root.value);
    if let Some(left) = &root.left {
        print_preorder(left);
    }
    if let Some(right) = &root.right {
        print_preorder(right);
    }
}

// Definir una función para imprimir un árbol binario en orden.
fn print_inorder(root: &Box<Node>) {
    if let Some(left) = &root.left {
        print_inorder(left);
    }
    print!("{} ", root.value);
    if let Some(right) = &root.right {
        print_inorder(right);
    }
}

// Definir una función para imprimir un árbol binario en postorden.
fn print_postorder(root: &Box<Node>) {
    if let Some(left) = &root.left {
        print_postorder(left);
    }
    if let Some(right) = &root.right {
        print_postorder(right);
    }
    print!("