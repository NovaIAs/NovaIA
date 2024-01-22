```go
// Paquete principal
package main

// Importar las bibliotecas necesarias
import (
	"fmt"
	"sync"
)

// Definir una estructura para representar un elemento de un árbol binario
type BinaryTreeNode struct {
	Value int
	Left  *BinaryTreeNode
	Right *BinaryTreeNode
}

// Definir una función para crear un nuevo nodo
func NewBinaryTreeNode(value int) *BinaryTreeNode {
	return &BinaryTreeNode{Value: value}
}

// Definir una función para insertar un nuevo nodo en un árbol binario
func InsertBinaryTree(root *BinaryTreeNode, value int) {
	if root == nil {
		root = NewBinaryTreeNode(value)
		return
	}

	if value < root.Value {
		InsertBinaryTree(root.Left, value)
	} else {
		InsertBinaryTree(root.Right, value)
	}
}

// Definir una función para buscar un nodo en un árbol binario
func SearchBinaryTree(root *BinaryTreeNode, value int) bool {
	if root == nil {
		return false
	}

	if root.Value == value {
		return true
	}

	if value < root.Value {
		return SearchBinaryTree(root.Left, value)
	} else {
		return SearchBinaryTree(root.Right, value)
	}
}

// Definir una función para calcular la altura de un árbol binario
func HeightBinaryTree(root *BinaryTreeNode) int {
	if root == nil {
		return -1
	}

	leftHeight := HeightBinaryTree(root.Left)
	rightHeight := HeightBinaryTree(root.Right)

	if leftHeight > rightHeight {
		return leftHeight + 1
	} else {
		return rightHeight + 1
	}
}

// Definir una función para imprimir un árbol binario en formato preorden
func PrintBinaryTreePreorder(root *BinaryTreeNode) {
	if root == nil {
		return
	}

	fmt.Print(root.Value, " ")
	PrintBinaryTreePreorder(root.Left)
	PrintBinaryTreePreorder(root.Right)
}

// Definir una función para imprimir un árbol binario en formato inorden
func PrintBinaryTreeInorder(root *BinaryTreeNode) {
	if root == nil {
		return
	}

	PrintBinaryTreeInorder(root.Left)
	fmt.Print(root.Value, " ")
	PrintBinaryTreeInorder(root.Right)
}

// Definir una función para imprimir un árbol binario en formato postorden
func PrintBinaryTreePostorder(root *BinaryTreeNode) {
	if root == nil {
		return
	}

	PrintBinaryTreePostorder(root.Left)
	PrintBinaryTreePostorder(root.Right)
	fmt.Print(root.Value, " ")
}

// Definir una función para eliminar un nodo de un árbol binario
func DeleteBinaryTree(root *BinaryTreeNode, value int) *BinaryTreeNode {
	if root == nil {
		return nil
	}

	if value < root.Value {
		root.Left = DeleteBinaryTree(root.Left, value)
	} else if value > root.Value {
		root.Right = DeleteBinaryTree(root.Right, value)
	} else {
		// Caso 1: Nodo sin hijos
		if root.Left == nil && root.Right == nil {
			root = nil
		}

		// Caso 2: Nodo con un solo hijo
		else if root.Left == nil {
			root = root.Right
		} else if root.Right == nil {
			root = root.Left
		}

		// Caso 3: Nodo con dos hijos
		else {
			// Encontrar el nodo más a la izquierda del subárbol derecho
			minNode := root.Right
			for minNode.Left != nil {
				minNode = minNode.Left
			}

			// Reemplazar el nodo a eliminar con el nodo más a la izquierda del subárbol derecho
			root.Value = minNode.Value

			// Eliminar el nodo más a la izquierda del subárbol derecho
			root.Right = DeleteBinaryTree(root.Right, minNode.Value)
		}
	}

	return root
}

// Función principal
func main() {
	// Crear una instancia de un árbol binario
	root := NewBinaryTreeNode(10)

	// Insertar algunos valores en el árbol binario
	InsertBinaryTree(root, 5)
	InsertBinaryTree(root, 15)
	InsertBinaryTree(root, 3)
	InsertBinaryTree(root, 7)
	InsertBinaryTree(root, 12)
	InsertBinaryTree(root, 20)

	// Buscar un valor en el árbol binario
	fmt.Println("Buscar el valor 12 en el árbol binario:", SearchBinaryTree(root, 12))

	// Calcular la altura del árbol binario
	fmt.Println("La altura del árbol binario es:", HeightBinaryTree(root))

	// Imprimir el árbol binario en formato preorden
	fmt.Println("Imprimir el árbol binario en formato preorden:")
	PrintBinaryTreePreorder(root)
	fmt.Println()

	// Imprimir el árbol binario en formato inorden
	fmt.Println("Imprimir el árbol binario en formato inorden:")
	PrintBinaryTreeInorder(root)
	fmt.Println()

	// Imprimir el árbol binario en formato postorden
	fmt.Println("Imprimir el árbol binario en formato postorden:")
	PrintBinaryTreePostorder(root)
	fmt.Println()

	// Eliminar un valor del árbol binario
	fmt.Println("Eliminar el valor 15 del árbol binario:")
	DeleteBinaryTree(root, 15)

	// Imprimir el árbol binario en formato preorden después de eliminar el valor 15
	fmt.Println("Imprimir el árbol binario en formato preorden después de eliminar el valor 15:")
	PrintBinaryTreePreorder(root)
	fmt.Println()
}
```

Explicación del código:

* La estructura `BinaryTreeNode` representa un nodo del árbol binario. Contiene un valor y punteros a sus nodos izquierdo y derecho.
* La función `NewBinaryTreeNode()` crea un nuevo nodo con el valor dado.
* La función `InsertBinaryTree()` inserta un nuevo nodo en el árbol binario. Si el nodo a insertar es menor que el valor del nodo actual, se inserta en el subárbol izquierdo. Si es mayor, se inserta en el subárbol derecho.
* La función `SearchBinaryTree()` busca un nodo en el árbol binario con el valor dado. Utiliza una búsqueda recursiva para recorrer el árbol y encontrar el nodo con el valor dado.
* La función `HeightBinaryTree()` calcula la altura del árbol binario. Utiliza una función recursiva para recorrer el árbol y encontrar la altura máxima de cualquier rama.
* Las funciones `PrintBinaryTreePreorder()`, `PrintBinaryTreeInorder()`, y `PrintBinaryTreePostorder()` imprimen el árbol binario en formato preorden, inorden y postorden respectivamente. Utilizan una función recursiva para recorrer el árbol y imprimir los valores de los nodos en el orden especificado.
* La función `DeleteBinaryTree()` elimina un nodo del árbol binario con el valor dado. Utiliza una función recursiva para recorrer el árbol y encontrar el nodo a eliminar. Una vez encontrado el nodo, lo elimina del árbol y ajusta los punteros de los nodos vecinos para mantener la estructura del árbol.

Este código es complejo porque implementa un árbol binario que permite insertar, buscar, calcular la altura, imprimir y eliminar nodos. Además, utiliza funciones recursivas para recorrer el árbol y realizar las operaciones necesarias. El código también es extenso porque proporciona una explicación detallada de cada función y de su funcionamiento.