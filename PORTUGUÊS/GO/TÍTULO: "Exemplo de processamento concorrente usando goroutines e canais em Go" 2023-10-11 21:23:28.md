Claro! Abaixo está um exemplo de um código complexo em Go, que envolve o uso de goroutines, canais e estruturas de dados personalizadas.

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

type Dados struct {
	chave string
	valor int
}

func processarDados(dados <-chan Dados, wg *sync.WaitGroup) {
	defer wg.Done()

	for dado := range dados {
		// Simula algum processamento complexo
		time.Sleep(500 * time.Millisecond)
		fmt.Printf("Processando dados com chave '%s' e valor %d\n", dado.chave, dado.valor)

		// Simula algum processamento adicional
		time.Sleep(300 * time.Millisecond)
		fmt.Printf("Finalizando processamento dos dados com chave '%s'\n", dado.chave)
	}
}

func main() {
	dados := []Dados{
		{chave: "A", valor: 10},
		{chave: "B", valor: 20},
		{chave: "C", valor: 30},
	}

	// Cria um canal para enviar os dados
	can