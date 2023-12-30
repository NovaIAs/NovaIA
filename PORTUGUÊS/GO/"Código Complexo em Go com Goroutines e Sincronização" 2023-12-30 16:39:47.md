Claro! Aqui está um exemplo de um código complexo em Go:

```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

func main() {
	const goroutines = 10
	const iterations = 100

	var wg sync.WaitGroup
	wg.Add(goroutines)

	rand.Seed(time.Now().UnixNano())

	for i := 0; i < goroutines; i++ {
		go func(g int) {
			defer wg.Done()

			for j := 0; j < iterations; j++ {
				value := generateRandomNumber()
				result := processValue(value)

				fmt.Printf("Goroutine %d, Iteration %d: Value %d processed to %d\n", g, j, value, result)

				time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
			}
		}(i)
	}

	wg.Wait()
	fmt.Println("All goroutines finished executing.")
}

func generateRandomNumber() int {
	return rand.Intn(1000)
}

func processValue(value int) int {
	// Exemplo de um algoritmo complexo para processar o valor
	result := value
	for i := 0; i < value; i++ {
		result = result * i
	}

	return result
}
```

Este código cria 10 goroutines que executam em paralelo, cada uma realizando 100 iterações. Cada goroutine gera um número aleatório, o processa usando uma função complexa `processValue()`, imprime o resultado e aguarda um tempo aleatório de espera antes de continuar para a próxima iteração.

A função `generateRandomNumber()` gera um número aleatório entre 0 e 999 usando a biblioteca `math/rand`.

A função `processValue()` recebe o valor gerado e realiza um processamento complexo, neste caso um loop em que o valor é multiplicado por cada número anterior até o valor original. Essa função é apenas um exemplo de um algoritmo complexo.

O código utiliza a sincronização do pacote `sync` para garantir que todas as goroutines terminem antes de imprimir a mensagem "All goroutines finished executing.".

Espero que este código complexo em Go atenda às suas expectativas!