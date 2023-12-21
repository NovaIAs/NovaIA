Claro! Aqui está um exemplo de código complexo em Go:

```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

var wg sync.WaitGroup

func main() {
	rand.Seed(time.Now().UnixNano()) // Semente para números aleatórios

	numThreads := 4 // Número de threads a serem executadas
	wg.Add(numThreads) // Adiciona as threads ao grupo de espera

	fmt.Println("Iniciando a execução das threads...")

	for i := 0; i < numThreads; i++ {
		go processThread(i)
	}

	wg.Wait() // Aguarda todas as threads terminarem a execução

	fmt.Println("Threads concluídas.")
}

func processThread(threadID int) {
	defer wg.Done() // Marca a thread como concluída quando a função terminar

	fmt.Printf("Iniciando a execução da thread %d\n", threadID)

	// Simula um processamento demorado
	randomDelay := rand.Intn(5) + 1 // Gera um atraso aleatório entre 1 e 5 segundos
	time.Sleep(time.Duration(randomDelay) * time.Second)

	fmt.Printf("Thread %d concluída.\n", threadID)
}
```

Este código cria um programa em Go que executa múltiplas threads de forma concorrente. A função `main` inicializa um número específico de threads (neste caso, 4) e as inicia chamando a função `processThread` em goroutines separadas. Cada thread simula um processamento demorado, com um atraso aleatório entre 1 e 5 segundos. Após o término de todas as threads, o programa exibe a mensagem "Threads concluídas.".

O código utiliza a biblioteca `sync` para criar um grupo de espera (`sync.WaitGroup`) e garantir que o programa principal aguarde a conclusão de todas as threads antes de encerrar. A função `rand.Seed` é utilizada para inicializar a semente dos números aleatórios com base no tempo atual.

Espero que este exemplo atenda às suas expectativas! Se você tiver alguma dúvida sobre o código, estou aqui para ajudar.