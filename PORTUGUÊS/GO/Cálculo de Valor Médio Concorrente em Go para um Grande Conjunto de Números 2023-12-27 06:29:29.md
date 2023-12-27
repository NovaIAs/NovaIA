Claro! Aqui está um código complexo em Go que usa concorrência para calcular o valor médio de um grande conjunto de números:

```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

const (
	numbersCount       = 10000000   // Número total de números a serem gerados
	concurrencyLevel   = 100        // Número de goroutines a serem executadas em paralelo
	numbersPerGoroutine = numbersCount / concurrencyLevel // Número de números que cada goroutine processará
)

func main() {
	// Inicializa o gerador de números aleatórios
	rand.Seed(time.Now().UnixNano())

	// Cria um canal para receber os resultados parciais de cada goroutine
	results := make(chan float64, concurrencyLevel)

	// Cria um grupo de espera para sincronização das goroutines
	var wg sync.WaitGroup
	wg.Add(concurrencyLevel)

	// Executa as goroutines em paralelo
	for i := 0; i < concurrencyLevel; i++ {
		go func() {
			defer wg.Done()

			// Calcula a soma dos números gerados pela goroutine atual
			sum := 0.0
			for j := 0; j < numbersPerGoroutine; j++ {
				sum += rand.Float64()
			}

			// Envia o resultado parcial para o canal
			results <- sum
		}()
	}

	// Aguarda todas as goroutines terminarem
	wg.Wait()

	// Fecha o canal para indicar que não haverá mais envios de resultados
	close(results)

	// Calcula o valor médio final
	totalSum := 0.0
	totalNumbers := 0
	for partialSum := range results {
		totalSum += partialSum
		totalNumbers += numbersPerGoroutine
	}

	average := totalSum / float64(totalNumbers)

	// Imprime o resultado
	fmt.Printf("O valor médio dos %d números gerados é: %.4f\n", numbersCount, average)
}
```

Este código em Go usa concorrência para gerar um grande conjunto de números aleatórios e calcular o valor médio desses números.

Primeiro, definimos algumas constantes, como o número total de números a serem gerados (`numbersCount`), o número de goroutines que serão executadas em paralelo (`concurrencyLevel`) e o número de números que cada goroutine processará (`numbersPerGoroutine`).

A seguir, criamos um canal chamado `results` para receber os resultados parciais de cada goroutine. Esse canal terá capacidade para armazenar os resultados de até `concurrencyLevel` goroutines.

Em seguida, criamos um grupo de espera (`sync.WaitGroup`) para sincronizar as goroutines. Chamamos o método `Add` do `WaitGroup` para indicar que `concurrencyLevel` goroutines serão executadas.

Dentro do loop `for`, criamos as goroutines em paralelo. Cada goroutine é responsável por gerar `numbersPerGoroutine` números aleatórios e calcular a soma desses números. O resultado parcial é enviado para o canal `results`.

Uma vez que todas as goroutines terminam, chamamos o método `Wait` do `WaitGroup` para aguardar que todas elas sejam concluídas.

Em seguida, fechamos o canal `results` para indicar que não haverá mais envios de resultados.

Por fim, percorremos os resultados parciais recebidos do canal `results` e somamos esses resultados ao valor total. Também contamos o número total de números processados. Finalmente, calculamos o valor médio dividindo a soma total pelo número total de números.

O resultado é impresso na tela utilizando a função `Printf` da biblioteca `fmt`.

Espero que este código atenda às suas expectativas de complexidade e diferenciação. Se tiver mais alguma solicitação, fique à vontade para pedir!