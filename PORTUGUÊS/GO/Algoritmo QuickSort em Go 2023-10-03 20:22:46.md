Claro, vou gerar um código complexo em Go para você. Este código é um exemplo de um algoritmo de ordenação chamado QuickSort.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Inicializa o gerador de números aleatórios
	rand.Seed(time.Now().UnixNano())

	// Cria um slice com números aleatórios
	numbers := generateRandomNumbers(10)

	fmt.Println("Slice não ordenado:", numbers)

	quickSort(numbers, 0, len(numbers)-1)

	fmt.Println("Slice ordenado:", numbers)
}

// Função para gerar números aleatórios
func generateRandomNumbers(size int) []int {
	numbers := make([]int, size)

	for i := 0; i < size; i++ {
		numbers[i] = rand.Intn(100)
	}

	return numbers
}

// Função de partição do QuickSort
func partition(numbers []int, low int, high int) int {
	pivot := numbers[high]
	i := low - 1

	for j := low; j < high; j++ {
		if numbers[j] <= pivot {
			i++
			numbers[i], numbers[j] = numbers[j], numbers[i]
		}
	}

	numbers[i+1], numbers[high] = numbers[high], numbers[i+1]

	return i + 1
}

// Função principal do QuickSort
func quickSort(numbers []int, low int, high int) {
	if low < high {
		pivot := partition(numbers, low, high)

		quickSort(numbers, low, pivot-1)
		quickSort(numbers, pivot+1, high)
	}
}
```

Este código implementa o algoritmo de ordenação QuickSort em Go. Ele começa criando um slice de números aleatórios usando a função `generateRandomNumbers`. Em seguida, o slice é exibido antes da ordenação.

O algoritmo QuickSort é implementado nas funções `partition` e `quickSort`. A função `partition` define um pivô e rearranja os elementos do slice de forma que todos os elementos menores que o pivô estejam à esquerda e todos os elementos maiores estejam à direita. A função `quickSort` divide o slice em partições menores e chama recursivamente a si mesma para classificar as partições.

Após a ordenação, o slice é exibido novamente para mostrar os números agora em ordem crescente.

Espero que este código complexo atenda às suas expectativas!