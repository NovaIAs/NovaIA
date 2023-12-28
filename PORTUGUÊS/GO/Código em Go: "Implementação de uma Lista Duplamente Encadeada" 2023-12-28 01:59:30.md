Claro! Aqui está um código complexo em Go que envolve a criação de uma estrutura de dados personalizada para gerenciar uma lista duplamente encadeada. Vou comentar cada linha para explicar o código:

```go
package main

import "fmt"

// Definindo a estrutura de um nó da lista duplamente encadeada
type Node struct {
    prev  *Node
    next  *Node
    value int
}

// Definindo a estrutura da lista duplamente encadeada
type DoublyLinkedList struct {
    head *Node
    tail *Node
}

// Função para adicionar um novo elemento no final da lista
func (list *DoublyLinkedList) Add(value int) {
    newNode := &Node{
        prev:  list.tail,
        next:  nil,
        value: value,
    }

    if list.head == nil {
        list.head = newNode
    } else {
        list.tail.next = newNode
    }

    list.tail = newNode
}

// Função para imprimir a lista na ordem
func (list *DoublyLinkedList) PrintForward() {
    current := list.head

    for current != nil {
        fmt.Printf("%d ", current.value)
        current = current.next
    }

    fmt.Println()
}

// Função para imprimir a lista em ordem reversa
func (list *DoublyLinkedList) PrintBackward() {
    current := list.tail

    for current != nil {
        fmt.Printf("%d ", current.value)
        current = current.prev
    }

    fmt.Println()
}

func main() {
    // Criando uma nova lista duplamente encadeada
    list := DoublyLinkedList{}

    // Adicionando elementos à lista
    list.Add(1)
    list.Add(2)
    list.Add(3)
    list.Add(4)
    list.Add(5)

    // Imprimindo a lista na ordem
    fmt.Println("Lista na ordem:")
    list.PrintForward()

    // Imprimindo a lista em ordem reversa
    fmt.Println("Lista em ordem reversa:")
    list.PrintBackward()
}
```

Este é um exemplo de código que cria uma lista duplamente encadeada em Go. A lista possui uma estrutura de nó que armazena um valor, uma referência para o próximo nó e uma referência para o nó anterior. A lista em si possui uma referência para a cabeça (primeiro nó) e para a cauda (último nó).

A função `Add` adiciona um novo elemento no final da lista, criando um novo nó e atualizando as referências adequadamente. A função `PrintForward` percorre a lista a partir da cabeça e imprime os valores na ordem. A função `PrintBackward` percorre a lista a partir da cauda e imprime os valores na ordem reversa.

No `main`, um exemplo de uso da lista é apresentado, adicionando alguns elementos e imprimindo a lista na ordem e em ordem reversa.

Este código é apenas um exemplo e pode ser modificado e adaptado para atender às necessidades específicas de um projeto. Espero que isso tenha lhe dado uma ideia de como implementar uma lista duplamente encadeada em Go.