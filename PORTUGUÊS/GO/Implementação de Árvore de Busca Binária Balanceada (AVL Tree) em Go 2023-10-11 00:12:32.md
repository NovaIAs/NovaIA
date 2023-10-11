Claro! Aqui está um código complexo em Go que utiliza uma estrutura de dados chamada árvore de busca binária balanceada (AVL Tree). Essa estrutura permite inserir, buscar e remover elementos de forma eficiente, mantendo sempre o balanceamento da árvore para garantir uma busca rápida.

```go
package main

import (
	"fmt"
)

// Definindo a estrutura do nó da árvore
type Node struct {
	Key    int
	Left   *Node
	Right  *Node
	Height int
}

// Função para calcular a altura de um nó
func height(node *Node) int {
	if node == nil {
		return 0
	}
	return node.Height
}

// Função para calcular o fator de balanceamento de um nó
func balanceFactor(node *Node) int {
	if node == nil {
		return 0
	}
	return height(node.Left) - height(node.Right)
}

// Função para atualizar a altura de um nó
func updateHeight(node *Node) {
	node.Height = 1 + max(height(node.Left), height(node.Right))
}

// Função auxiliar para retornar o máximo entre dois inteiros
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Função para realizar a rotação simples à esquerda
func rotateLeft(node *Node) *Node {
	rightChild := node.Right
	node.Right = rightChild.Left
	rightChild.Left = node
	updateHeight(node)
	updateHeight(rightChild)
	return rightChild
}

// Função para realizar a rotação simples à direita
func rotateRight(node *Node) *Node {
	leftChild := node.Left
	node.Left = leftChild.Right
	leftChild.Right = node
	updateHeight(node)
	updateHeight(leftChild)
	return leftChild
}

// Função para balancear a árvore após uma inserção ou remoção
func balance(node *Node) *Node {
	if node == nil {
		return nil
	}
	updateHeight(node)
	balanceFactor := balanceFactor(node)

	// Caso esquerda-esquerda
	if balanceFactor > 1 && balanceFactor(node.Left) >= 0 {
		return rotateRight(node)
	}

	// Caso esquerda-direita
	if balanceFactor > 1 && balanceFactor(node.Left) < 0 {
		node.Left = rotateLeft(node.Left)
		return rotateRight(node)
	}

	// Caso direita-direita
	if balanceFactor < -1 && balanceFactor(node.Right) <= 0 {
		return rotateLeft(node)
	}

	// Caso direita-esquerda
	if balanceFactor < -1 && balanceFactor(node.Right) > 0 {
		node.Right = rotateRight(node.Right)
		return rotateLeft(node)
	}

	return node
}

// Função para inserir um valor na árvore
func insert(node *Node, key int) *Node {
	if node == nil {
		return &Node{Key: key, Height: 1}
	}

	if key < node.Key {
		node.Left = insert(node.Left, key)
	} else if key > node.Key {
		node.Right = insert(node.Right, key)
	} else {
		return node // chave duplicada, não faz nada
	}

	return balance(node)
}

// Função para buscar um valor na árvore
func search(node *Node, key int) *Node {
	if node == nil || node.Key == key {
		return node
	}

	if key < node.Key {
		return search(node.Left, key)
	}

	return search(node.Right, key)
}

// Função para remover um valor da árvore
func remove(node *Node, key int) *Node {
	if node == nil {
		return nil
	}

	if key < node.Key {
		node.Left = remove(node.Left, key)
	} else if key > node.Key {
		node.Right = remove(node.Right, key)
	} else {
		// nó encontrado
		if node.Left == nil && node.Right == nil {
			node = nil
		} else if node.Left == nil {
			node = node.Right
		} else if node.Right == nil {
			node = node.Left
		} else {
			successor := findMin(node.Right)
			node.Key = successor.Key
			node.Right = remove(node.Right, successor.Key)
		}
	}

	return balance(node)
}

// Função auxiliar para encontrar o nó com o menor valor na subárvore direita
func findMin(node *Node) *Node {
	for node.Left != nil {
		node = node.Left
	}
	return node
}

// Função para imprimir a árvore em ordem crescente
func printInOrder(node *Node) {
	if node != nil {
		printInOrder(node.Left)
		fmt.Printf("%d ", node.Key)
		printInOrder(node.Right)
	}
}

func main() {
	var root *Node

	// Inserção de valores na árvore
	root = insert(root, 10)
	root = insert(root, 20)
	root = insert(root, 30)
	root = insert(root, 40)
	root = insert(root, 50)
	root = insert(root, 25)

	// Impressão em ordem crescente
	fmt.Println("Árvore em ordem crescente:")
	printInOrder(root)
	fmt.Println()

	// Busca de valores
	fmt.Println("Busca na árvore:")
	node := search(root, 30)
	if node != nil {
		fmt.Println("Valor encontrado!")
	} else {
		fmt.Println("Valor não encontrado!")
	}

	// Remoção de valores
	root = remove(root, 30)
	fmt.Println("Árvore após a remoção:")
	printInOrder(root)
}
```

Este código implementa a estrutura de dados AVL Tree em Go. A árvore é balanceada automaticamente a cada inserção e remoção, garantindo um tempo de busca eficiente. O programa demonstra a inserção de valores na árvore, a busca por um valor específico e a remoção de valores. No final, a árvore é impressa em ordem crescente.