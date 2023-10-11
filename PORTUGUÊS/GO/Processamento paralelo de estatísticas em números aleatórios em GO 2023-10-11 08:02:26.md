Claro! Aqui está um exemplo de um código complexo em GO:

```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

func main() {
	// Gerar uma seed única para o gerador de números aleatórios
	rand.Seed(time.Now().UnixNano())

	// Criar um slice de números aleatórios
	numbers := generateRandomNumbers(1000)

	// Criar um mapa para armazenar as estatísticas de cada número
	stats := make(map[int]Statistics)

	// Criar um canal para comunicação entre as goroutines
	ch := make(chan int)

	// Criar um grupo de wait para esperar todas as goroutines terminarem
	var wg sync.WaitGroup

	// Iniciar 10 goroutines para processar os números em paralelo
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go processNumber(ch, &wg, stats)
	}

	// Enviar os números para o canal
	for _, num := range numbers {
		ch <- num
	}

	// Fechar o canal para indicar que não há mais números a serem processados
	close(ch)

	// Aguardar todas as goroutines terminarem
	wg.Wait()

	// Imprimir as estatísticas dos números processados
	for num, stat := range stats {
		fmt.Printf("Número: %d\n", num)
		fmt.Printf("Média: %.2f\n", stat.Average)
		fmt.Printf("Soma: %d\n", stat.Sum)
		fmt.Printf("Quantidade: %d\n", stat.Count)
		fmt.Println("--------")
	}
}

// Função para gerar uma slice de números aleatórios
func generateRandomNumbers(n int) []int {
	numbers := make([]int, n)
	for i := 0; i < n; i++ {
		numbers[i] = rand.Intn(100)
	}
	return numbers
}

// Função para processar um número e atualizar as estatísticas
func processNumber(ch chan int, wg *sync.WaitGroup, stats map[int]Statistics) {
	for num := range ch {
		// Atualizar as estatísticas do número
		stat := stats[num]
		stat.Count++
		stat.Sum += num
		stat.Average = float64(stat.Sum) / float64(stat.Count)
		stats[num] = stat
	}
	wg.Done()
}

// Estrutura para armazenar as estatísticas de um número
type Statistics struct {
	Count   int
	Sum     int
	Average float64
}
```

Neste código em GO, temos um exemplo de um sistema que processa um grande conjunto de números aleatórios de forma paralela e calcula as estatísticas de cada número, incluindo a média, a soma e a quantidade de ocorrências.

O programa começa gerando uma seed única para o gerador de números aleatórios, utilizando o valor atual do tempo em nanossegundos. Em seguida, é criado um slice com 1000 números aleatórios utilizando a função `generateRandomNumbers`.

É criado um mapa chamado `stats` para armazenar as estatísticas de cada número. Em seguida, é criado um canal chamado `ch` para comunicação entre as goroutines, e um grupo de wait chamado `wg` para aguardar todas as goroutines terminarem.

O programa inicia 10 goroutines utilizando a função `processNumber`, passando o canal, o grupo de wait e o mapa de estatísticas como parâmetros. Cada goroutine irá processar os números recebidos pelo canal, atualizando as estatísticas correspondentes no mapa.

Em seguida, os números aleatórios são enviados para o canal utilizando o loop `for range`. Após o envio dos números, o canal é fechado para indicar que não há mais números a serem processados.

O programa aguarda todas as goroutines terminarem utilizando a função `wg.Wait()`. Em seguida, imprime as estatísticas de cada número processado utilizando um loop `for range` no mapa `stats`.

Por fim, temos a definição da função `generateRandomNumbers` para gerar uma slice de números aleatórios, e a definição da função `processNumber` para processar um número e atualizar as estatísticas. Além disso, temos a definição da estrutura `Statistics` para armazenar as estatísticas de um número.