Claro! Aqui está um exemplo de um código complexo em Go, que utiliza concorrência para realizar o processamento de um conjunto de tarefas:

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

const (
	numTarefas  = 10   // Número total de tarefas
	numTrabalhos = 100  // Número total de trabalhos para cada tarefa
)

func main() {
	// Criar um canal para enviar trabalhos para as tarefas
	trabalhos := make(chan int, numTrabalhos)

	// Iniciar as tarefas
	var wg sync.WaitGroup
	for i := 0; i < numTarefas; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			for trabalho := range trabalhos {
				processarTrabalho(id, trabalho)
			}
		}(i)
	}

	// Enviar trabalhos para as tarefas
	for i := 0; i < numTrabalhos; i++ {
		trabalhos <- i
	}
	close(trabalhos)

	// Aguardar a conclusão de todas as tarefas
	wg.Wait()

	fmt.Println("Todos os trabalhos foram processados.")
}

func processarTrabalho(idTarefa, trabalho int) {
	// Simular um processamento demorado
	time.Sleep(time.Millisecond * time.Duration(500+trabalho%500))
	fmt.Printf("Tarefa %d processou o trabalho %d\n", idTarefa, trabalho)
}
```

Neste exemplo, temos um programa que simula a execução de um conjunto de tarefas concorrentemente. Cada tarefa é responsável por processar um conjunto de trabalhos. O número total de tarefas e trabalhos é definido pelas constantes `numTarefas` e `numTrabalhos`.

O programa utiliza um canal chamado `trabalhos` para enviar os trabalhos para as tarefas. As tarefas são criadas em uma goroutine separada para possibilitar a execução concorrente. Cada tarefa recebe trabalhos do canal e os processa chamando a função `processarTrabalho`.

A função `processarTrabalho` simula um processamento demorado ao esperar um tempo aleatório entre 500 e 999 milissegundos. Após o processamento, a tarefa imprime uma mensagem indicando qual trabalho foi processado.

Ao final do programa, é exibida uma mensagem informando que todos os trabalhos foram processados.