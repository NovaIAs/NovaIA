Claro! Aqui está um exemplo de um código complexo em Go, que utiliza concorrência para processar uma lista de números primos:

```go
package main

import (
	"fmt"
	"math"
	"sync"
)

func main() {
	// Define a quantidade de goroutines a serem usadas
	numGoroutines := 4

	// Define o número máximo para encontrar os primos
	maxNumber := 100000

	// Cria um canal para receber os números primos encontrados
	primes := make(chan int)

	// Cria um WaitGroup para sincronizar as goroutines
	var wg sync.WaitGroup
	wg.Add(numGoroutines)

	// Divide a faixa de números em partes iguais para cada goroutine processar
	partSize := maxNumber / numGoroutines

	// Executa as goroutines para encontrar os números primos
	for i := 0; i < numGoroutines; i++ {
		start := i * partSize
		end := start + partSize

		go findPrimes(start, end, primes, &wg)
	}

	// Inicia uma goroutine para fechar o canal depois que todas as goroutines terminarem
	go func() {
		wg.Wait()
		close(primes)
	}()

	// Imprime os números primos encontrados
	for prime := range primes {
		fmt.Println(prime)
	}
}

// Função para encontrar números primos em uma faixa específica
func findPrimes(start, end int, primes chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()

	for i := start; i < end; i++ {
		if isPrime(i) {
			primes <- i
		}
	}
}

// Função para verificar se um número é primo
func isPrime(n int) bool {
	if n <= 1 {
		return false
	}

	for i := 2; i <= int(math.Sqrt(float64(n))); i++ {
		if n%i == 0 {
			return false
		}
	}

	return true
}
```

Neste código, utilizamos concorrência para dividir a tarefa de encontrar números primos em várias goroutines. A quantidade de goroutines é definida pela variável `numGoroutines`, e a faixa de números a serem verificados é dividida igualmente entre as goroutines.

A função `findPrimes` é executada por cada goroutine, recebendo uma faixa de números para verificar se são primos. A função `isPrime` é utilizada para verificar se um número é primo ou não.

O canal `primes` é utilizado para receber os números primos encontrados por todas as goroutines. Uma vez que todas as goroutines terminam, o canal é fechado e os números primos são impressos na função `main`.

Este código demonstra o uso de concorrência em Go para processar uma tarefa complexa de forma mais eficiente. O uso de goroutines permite que várias tarefas sejam executadas em paralelo, melhorando o desempenho do programa.