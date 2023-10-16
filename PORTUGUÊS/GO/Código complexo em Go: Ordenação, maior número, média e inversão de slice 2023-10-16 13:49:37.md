Claro! Aqui está um exemplo de código complexo em Go:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())

	// Cria um slice com valores aleatórios
	numbers := generateRandomNumbers(10, 1, 100)
	fmt.Println("Slice original:", numbers)

	// Ordena o slice de forma crescente usando o algoritmo Bubble Sort
	bubbleSort(numbers)
	fmt.Println("Slice ordenado:", numbers)

	// Encontra o maior número no slice
	max := findMax(numbers)
	fmt.Println("Maior número:", max)

	// Calcula a média dos números no slice
	average := calculateAverage(numbers)
	fmt.Println("Média:", average)

	// Inverte o slice
	reverseSlice(numbers)
	fmt.Println("Slice invertido:", numbers)
}

// Função para gerar números aleatórios no intervalo [min, max]
func generateRandomNumbers(length, min, max int) []int {
	numbers := make([]int, length)
	for i := 0; i < length; i++ {
		numbers[i] = rand.Intn(max-min+1) + min
	}
	return numbers
}

// Função para ordenar um slice usando Bubble Sort
func bubbleSort(numbers []int) {
	n := len(numbers)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			if numbers[j] > numbers[j+1] {
				numbers[j], numbers[j+1] = numbers[j+1], numbers[j]
			}
		}
	}
}

// Função para encontrar o maior número em um slice
func findMax(numbers []int) int {
	max := numbers[0]
	for _, num := range numbers {
		if num > max {
			max = num
		}
	}
	return max
}

// Função para calcular a média dos números em um slice
func calculateAverage(numbers []int) float64 {
	sum := 0
	for _, num := range numbers {
		sum += num
	}
	return float64(sum) / float64(len(numbers))
}

// Função para inverter um slice
func reverseSlice(numbers []int) {
	n := len(numbers)
	for i := 0; i < n/2; i++ {
		numbers[i], numbers[n-1-i] = numbers[n-1-i], numbers[i]
	}
}
```

Este código em Go demonstra várias funcionalidades diferentes. Primeiro, ele gera um slice de números aleatórios usando a função `generateRandomNumbers`. Em seguida, o algoritmo Bubble Sort é utilizado para ordenar o slice em ordem crescente. A função `findMax` encontra o maior número no slice e a função `calculateAverage` calcula a média dos números. Por fim, a função `reverseSlice` inverte a ordem dos elementos no slice.

Este código é complexo porque combina várias funcionalidades diferentes e utiliza algoritmos de ordenação e manipulação de slices. Ele demonstra o uso de loops, condicionais, funções e manipulação de dados em Go.