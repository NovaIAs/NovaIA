Claro! Aqui está um código em Go que implementa uma árvore binária de pesquisa (BST) e realiza algumas operações comuns nessa estrutura de dados.

```go
package main

import (
	"fmt"
)

// Definindo a estrutura do nó da árvore
type Node struct {
	Key   int
	Left  *Node
	Right *Node
}

// Função para inserir um novo nó na árvore
func Insert(root *Node, key int) *Node {
	if root == nil {
		return &Node{Key: key, Left: nil, Right: nil}
	}

	if key < root.Key {
		root.Left = Insert(root.Left, key)
	} else if key > root.Key {
		root.Right = Insert(root.Right, key)
	}

	return root
}

// Função para verificar se um determinado valor está presente na árvore
func Search(root *Node, key int) bool {
	if root == nil {
		return false
	}

	if key == root.Key {
		return true
	} else if key < root.Key {
		return Search(root.Left, key)
	} else {
		return Search(root.Right, key)
	}
}

// Função para realizar a travessia em ordem (In-Order) da árvore
func InOrderTraversal(root *Node) {
	if root != nil {
		InOrderTraversal(root.Left)
		fmt.Printf("%d ", root.Key)
		InOrderTraversal(root.Right)
	}
}

// Função principal
func main() {
	// Criando a raiz da árvore
	root := Insert(nil, 50)

	// Inserindo nós na árvore
	Insert(root, 30)
	Insert(root, 20)
	Insert(root, 40)
	Insert(root, 70)
	Insert(root, 60)
	Insert(root, 80)

	// Verificando se um valor está presente na árvore
	fmt.Println("Valor 20 está presente na árvore?", Search(root, 20))
	fmt.Println("Valor 90 está presente na árvore?", Search(root, 90))

	// Realizando a travessia em ordem da árvore
	fmt.Println("Travessia em ordem (In-Order):")
	InOrderTraversal(root)
}
```

Neste código, implementei uma estrutura de dados de árvore binária de pesquisa (BST) em Go. 

A função `Insert` é responsável por inserir um novo nó na árvore. Ela verifica se a árvore está vazia e, se estiver, cria um novo nó com a chave fornecida. Caso contrário, a função faz uma comparação entre a chave a ser inserida e a chave do nó atual para decidir se o novo nó deve ser inserido na subárvore esquerda ou direita do nó atual. A função é chamada recursivamente até encontrar a posição correta para inserir o novo nó.

A função `Search` é usada para verificar se um determinado valor está presente na árvore. Ela também utiliza recursão para percorrer a árvore, comparando a chave fornecida com a chave do nó atual. Se as chaves forem iguais, a função retorna verdadeiro. Caso contrário, a função chama recursivamente a si mesma na subárvore esquerda ou direita, dependendo da comparação.

A função `InOrderTraversal` realiza a travessia em ordem (In-Order) da árvore, ou seja, visita os nós da árvore em ordem crescente. Ela utiliza recursão para percorrer a árvore, visitando o nó da subárvore esquerda, em seguida o nó atual e, por fim, o nó da subárvore direita.

Na função principal (`main`), criamos a raiz da árvore e inserimos alguns nós. Em seguida, verificamos se determinados valores estão presentes na árvore utilizando a função `Search` e realizamos a travessia em ordem da árvore utilizando a função `InOrderTraversal`.