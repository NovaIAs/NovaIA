Claro! Aqui está um código em Go que implementa uma árvore de busca binária. A árvore consiste em nós que contêm um valor inteiro, bem como uma referência para seus nós filho esquerdo e direito. A função principal cria uma árvore de exemplo, insere alguns valores e, em seguida, realiza uma busca na árvore para verificar se um valor específico está presente.

```go
package main

import "fmt"

// Definindo a estrutura para o nó da árvore
type Node struct {
    value       int
    left, right *Node
}

// Inserindo um valor em uma árvore
func Insert(root *Node, value int) *Node {
    if root == nil {
        return &Node{value, nil, nil}
    }

    if value < root.value {
        root.left = Insert(root.left, value)
    } else if value > root.value {
        root.right = Insert(root.right, value)
    }

    return root
}

// Buscando um valor na árvore
func Search(root *Node, value int) bool {
    if root == nil {
        return false
    }

    if root.value == value {
        return true
    } else if value < root.value {
        return Search(root.left, value)
    } else {
        return Search(root.right, value)
    }
}

func main() {
    // Criando a árvore de exemplo
    root := &Node{10, nil, nil}
    Insert(root, 5)
    Insert(root, 15)
    Insert(root, 3)
    Insert(root, 8)
    Insert(root, 12)
    Insert(root, 20)

    // Buscando valores na árvore
    fmt.Println(Search(root, 8))   // true
    fmt.Println(Search(root, 12))  // true
    fmt.Println(Search(root, 25))  // false
    fmt.Println(Search(root, 2))   // false
}
```

Neste código, a função `Insert` permite inserir um novo valor na árvore, enquanto a função `Search` permite buscar um valor específico na árvore. A função `main` cria uma árvore de exemplo com alguns valores e, em seguida, realiza a busca por alguns valores específicos.