Claro! Aqui está um código complexo em Rust que envolve a manipulação de strings, estruturas de dados e recursão:

```rust
// Definindo uma estrutura de dados para representar uma árvore binária
struct BinaryTree<T> {
    value: T,
    left: Option<Box<BinaryTree<T>>>,
    right: Option<Box<BinaryTree<T>>>,
}

// Função para criar uma árvore binária a partir de um vetor de valores
fn create_binary_tree<T>(values: &[T]) -> Option<Box<BinaryTree<T>>> {
    if values.is_empty() {
        return None;
    }

    let mid = values.len() / 2;
    let value = values[mid];

    let left = create_binary_tree(&values[..mid]);
    let right = create_binary_tree(&values[mid + 1..]);

    Some(Box::new(BinaryTree {
        value,
        left,
        right,
    }))
}

// Função para imprimir em ordem simétrica os valores de uma árvore binária
fn inorder_traversal<T>(tree: &Option<Box<BinaryTree<T>>>) {
    if let Some(node) = tree {
        inorder_traversal(&node.left);
        println!("{}", node.value);
        inorder_traversal(&node.right);
    }
}

fn main() {
    let values = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let tree = create_binary_tree(&values);
    inorder_traversal(&tree);
}
```

Este código cria uma estrutura de dados chamada `BinaryTree`, que representa uma árvore binária. Em seguida, ele define uma função `create_binary_tree` que recebe um vetor de valores e retorna uma árvore binária construída a partir desses valores. A função usa recursão para dividir o vetor ao meio e criar subárvores esquerda e direita.

O programa principal cria um vetor de números de 1 a 10 e usa a função `create_binary_tree` para construir a árvore correspondente. Em seguida, a função `inorder_traversal` é chamada para imprimir os valores da árvore em ordem simétrica.

Espero que você goste deste código complexo em Rust!