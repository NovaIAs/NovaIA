Claro! Aqui está um código complexo em Go que usa goroutines para executar tarefas em paralelo:

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	// Definindo o número de goroutines
	numGoroutines := 5

	// Criando um wait group para esperar todas as goroutines terminarem
	var wg sync.WaitGroup
	wg.Add(numGoroutines)

	// Criando um canal para receber os resultados das goroutines
	resultados := make(chan int, numGoroutines)

	// Criando as goroutines
	for i := 0; i < numGoroutines; i++ {
		go func(id int) {
			defer wg.Done()

			// Executando uma tarefa simulada
			resultado := executarTarefa(id)

			// Enviando o resultado para o canal
			resultados <- resultado
		}(i)
	}

	// Fechando o canal após todas as goroutines terminarem
	go func() {
		wg.Wait()
		close(resultados)
	}()

	// Lendo os resultados do canal e somando-os
	soma := 0
	for resultado := range resultados {
		soma += resultado
	}

	// Imprimindo a soma final
	fmt.Println("Soma dos resultados:", soma)
}

func executarTarefa(id int) int {
	// Simulação de uma tarefa complexa
	fmt.Printf("Goroutine %d: executando tarefa\n", id)
	resultado := id * 2
	return resultado
}
```

Este código cria um número definido de goroutines (no exemplo, 5) para executar uma tarefa simulada de forma paralela. Cada goroutine recebe um ID único e executa a função `executarTarefa` para gerar um resultado. Os resultados são enviados para um canal e depois somados. No final, a soma total é impressa. 

O código utiliza o pacote `sync` para criar um WaitGroup, que permite esperar todas as goroutines terminarem antes de prosseguir. Além disso, é utilizado um canal para enviar os resultados das goroutines e uma goroutine adicional para fechar o canal após todas as goroutines terem terminado.

Ao executar o código, você verá mensagens indicando a execução das goroutines, bem como a soma final dos resultados. Este exemplo demonstra o uso de concorrência em Go e como executar tarefas em paralelo.