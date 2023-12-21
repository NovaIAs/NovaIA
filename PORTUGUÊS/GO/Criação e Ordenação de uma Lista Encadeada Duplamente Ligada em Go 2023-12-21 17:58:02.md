Claro! Aqui está um código complexo em Go que cria uma lista encadeada duplamente ligada e executa uma operação de ordenação nela:

```go
package main

import (
	"fmt"
)

type Node struct {
	prev  *Node
	next  *Node
	value int
}

type LinkedList struct {
	head *Node
	tail *Node
}

func (ll *LinkedList) Insert(value int) {
	node := &Node{
		prev:  ll.tail,
		value: value,
	}

	if ll.head == nil {
		ll.head = node
		ll.tail = node
	} else {
		ll.tail.next = node
		ll.tail = node
	}
}

func (ll *LinkedList) Sort() {
	if ll.head == nil || ll.head == ll.tail {
		return
	}

	len := ll.Length()

	for i := 0; i < len-1; i++ {
		current := ll.head
		next := current.next

		for next != nil {
			if current.value > next.value {
				ll.swap(current, next)
			}

			current = next
			next = next.next
		}
	}
}

func (ll *LinkedList) swap(node1 *Node, node2 *Node) {
	temp := node1.value
	node1.value = node2.value
	node2.value = temp
}

func (ll *LinkedList) Length() int {
	count := 0
	current := ll.head

	for current != nil {
		count++
		current = current.next
	}

	return count
}

func (ll *LinkedList) Print() {
	current := ll.head

	for current != nil {
		fmt.Printf("%d ", current.value)
		current = current.next
	}

	fmt.Println()
}

func main() {
	ll := LinkedList{}

	ll.Insert(10)
	ll.Insert(5)
	ll.Insert(8)
	ll.Insert(3)
	ll.Insert(1)

	fmt.Println("Lista não ordenada:")
	ll.Print()

	ll.Sort()

	fmt.Println("Lista ordenada:")
	ll.Print()
}
```

Neste código, começamos definindo a estrutura `Node`, que representa cada nó na lista encadeada duplamente ligada. Cada nó possui um ponteiro para o nó anterior (`prev`), um ponteiro para o próximo nó (`next`) e um valor inteiro (`value`).

Em seguida, temos a estrutura `LinkedList`, que possui um ponteiro para o primeiro nó (`head`) e um ponteiro para o último nó (`tail`). Essa estrutura também contém métodos para inserir um valor na lista (`Insert`), ordenar a lista (`Sort`), obter o tamanho da lista (`Length`) e imprimir os valores da lista (`Print`).

No método `Insert`, criamos um novo nó com o valor fornecido e o inserimos no final da lista. Se a lista estiver vazia, o nó criado será tanto o primeiro quanto o último nó da lista.

No método `Sort`, realizamos a ordenação da lista usando o algoritmo de ordenação bubble sort. Percorremos a lista várias vezes, comparando cada nó com o próximo e trocando-os de posição se necessário.

O método `swap` é usado para trocar os valores entre dois nós.

O método `Length` calcula o tamanho da lista percorrendo todos os nós e contando-os.

O método `Print` imprime os valores da lista.

Na função `main`, criamos uma instância de `LinkedList` e inserimos alguns valores. Em seguida, imprimimos a lista não ordenada, chamamos o método `Sort` e imprimimos a lista ordenada.
