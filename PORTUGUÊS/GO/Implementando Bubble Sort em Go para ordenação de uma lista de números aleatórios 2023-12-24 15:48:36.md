Claro! Aqui está um exemplo de um código em Go que implementa um algoritmo de ordenação chamado Bubble Sort:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Gerando uma lista aleatória de números
	rand.Seed(time.Now().UnixNano())
	numbers := generateRandomNumbers(10)

	fmt.Println("Lista não ordenada:", numbers)

	// Ordenando a lista usando Bubble Sort
	bubbleSort(numbers)

	fmt.Println("Lista ordenada:", numbers)
}

func generateRandomNumbers(n int) []int {
	numbers := make([]int, n)
	for i := 0; i < n; i++ {
		numbers[i] = rand.Intn(100)
	}
	return numbers
}

func bubbleSort(numbers []int) {
	n := len(numbers)
	swapped := true

	for swapped {
		swapped = false

		for i := 1; i < n; i++ {
			if numbers[i-1] > numbers[i] {
				// Trocar os elementos de posição
				numbers[i-1], numbers[i] = numbers[i], numbers[i-1]
				swapped = true
			}
		}
		n--
	}
}
```

Este código começa importando os pacotes necessários, `fmt` para impressão e formatação, `math/rand` para gerar números aleatórios e `time` para obter o tempo atual como semente para a geração aleatória.

A função `main` é onde o programa principal é executado. Começamos gerando uma lista de 10 números aleatórios usando a função `generateRandomNumbers`. Em seguida, imprimimos a lista não ordenada.

A função `generateRandomNumbers` cria um slice de tamanho `n` e preenche cada elemento com um número aleatório entre 0 e 100.

A função `bubbleSort` implementa o algoritmo Bubble Sort. O loop externo executa até que nenhum elemento seja trocado, indicando que a lista está ordenada. O loop interno compara pares de elementos adjacentes e troca-os de posição se estiverem fora de ordem. A variável `n` é usada para controlar o número de iterações necessárias.

No final, a lista ordenada é impressa.

Espero que este exemplo atenda às suas expectativas!