Claro! Aqui está um código complexo em Go que utiliza concorrência para calcular a soma de todos os números em um intervalo definido pelo usuário. O código é comentado em português para facilitar o entendimento:

```go
package main

import (
	"fmt"
	"sync"
)

// Função para calcular a soma dos números em um intervalo específico
func calcularSoma(inicio, fim int, wg *sync.WaitGroup, soma *int) {
	defer wg.Done() // Indica que a goroutine finalizou

	// Variável local para armazenar a soma dos números no intervalo
	var total int

	// Loop para somar os números no intervalo
	for i := inicio; i <= fim; i++ {
		total += i
	}

	// Adiciona a soma parcial à soma total
	*soma += total
}

func main() {
	// Define o número de goroutines
	numGoroutines := 4

	// Define o intervalo de números a serem somados
	inicio := 1
	fim := 100

	// Calcula o tamanho do intervalo para cada goroutine
	tamanhoIntervalo := (fim - inicio + 1) / numGoroutines

	// Cria um WaitGroup para sincronizar as goroutines
	var wg sync.WaitGroup

	// Cria uma variável para armazenar a soma total
	somaTotal := 0

	// Loop para criar as goroutines
	for i := 0; i < numGoroutines; i++ {
		// Incrementa o WaitGroup para cada goroutine criada
		wg.Add(1)

		// Calcula o início e fim do intervalo para cada goroutine
		goroutineInicio := inicio + (i * tamanhoIntervalo)
		goroutineFim := goroutineInicio + tamanhoIntervalo - 1

		// Verifica se é a última goroutine e ajusta o fim do intervalo, se necessário
		if i == numGoroutines-1 {
			goroutineFim = fim
		}

		// Chama a função para calcular a soma parcial
		go calcularSoma(goroutineInicio, goroutineFim, &wg, &somaTotal)
	}

	// Aguarda todas as goroutines finalizarem
	wg.Wait()

	// Imprime a soma total dos números no intervalo
	fmt.Printf("A soma total dos números no intervalo de %d a %d é: %d\n", inicio, fim, somaTotal)
}
```

Neste código, utilizamos a linguagem de programação Go para somar todos os números em um intervalo definido pelo usuário. O cálculo da soma é distribuído entre várias goroutines, que são executadas simultaneamente para melhorar o desempenho.

Primeiro, definimos o número de goroutines que serão criadas (neste exemplo, 4) e o intervalo de números a ser somado (de 1 a 100). Em seguida, calculamos o tamanho do intervalo para cada goroutine.

Utilizamos um WaitGroup para sincronizar as goroutines e garantir que todas terminem antes de prosseguirmos. Criamos uma variável somaTotal para armazenar a soma total dos números.

Em seguida, utilizamos um loop para criar as goroutines. Incrementamos o WaitGroup para cada goroutine criada e calculamos os valores de início e fim do intervalo para cada uma. Se for a última goroutine, ajustamos o fim do intervalo para evitar que números sejam ignorados.

Dentro da função calcularSoma, utilizamos um loop para somar os números no intervalo definido. A soma parcial é adicionada à soma total por meio de um ponteiro.

Após a criação de todas as goroutines, utilizamos o método Wait() do WaitGroup para aguardar a finalização de todas elas. Em seguida, imprimimos a soma total dos números no intervalo.

Espero que este código atenda às suas expectativas!