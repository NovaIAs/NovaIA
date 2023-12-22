Claro! Aqui está um código complexo em Go, que implementa um algoritmo de busca em largura (Breadth-First Search) em um grafo:

```go
package main

import (
	"fmt"
)

// Estrutura que representa um nó do grafo
type Node struct {
	ID       int
	Children []*Node
}

// Função para realizar a busca em largura no grafo
func BFS(root *Node) {
	queue := []*Node{root} // Fila para armazenar os nós a serem visitados
	visited := make(map[*Node]bool)

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]

		if !visited[node] {
			fmt.Printf("Visitando nó %d\n", node.ID)
			visited[node] = true

			for _, child := range node.Children {
				queue = append(queue, child)
			}
		}
	}
}

func main() {
	// Criação do grafo de exemplo
	node1 := &Node{ID: 1}
	node2 := &Node{ID: 2}
	node3 := &Node{ID: 3}
	node4 := &Node{ID: 4}
	node5 := &Node{ID: 5}

	node1.Children = []*Node{node2, node3}
	node2.Children = []*Node{node4}
	node3.Children = []*Node{node4, node5}

	// Chamada da função de busca em largura
	BFS(node1)
}
```

Nesse código, temos uma estrutura `Node` que representa um nó no grafo. Cada nó possui um ID único e uma lista de nós filhos. A função `BFS` implementa o algoritmo de busca em largura, utilizando uma fila para armazenar os nós a serem visitados. A medida que os nós são visitados, eles são marcados como visitados e seus filhos são adicionados à fila. A função `main` cria um grafo de exemplo e chama a função `BFS` para realizar a busca em largura a partir do nó raiz.

Espero que esse código atenda às suas expectativas!