Claro! Aqui está um código complexo em Go que envolve o uso de concorrência e canais para realizar cálculos paralelos. O código a seguir é um exemplo de um simulador de corrida de carros com múltiplos competidores:

```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type Carro struct {
	ID    int
	Velocidade int
}

func main() {
	numCarros := 5
	var wg sync.WaitGroup
	wg.Add(numCarros)

	canalCarros := make(chan Carro)

	vencedor := make(chan Carro)

	go func() {
		vencedor := <-vencedor
		fmt.Printf("Carro %d venceu a corrida!\n", vencedor.ID)
	}()

	for i := 0; i < numCarros; i++ {
		go func(id int) {
			defer wg.Done()

			carro := Carro{
				ID:    id,
				Velocidade: rand.Intn(100),
			}

			canalCarros <- carro

			if carro.Velocidade > vencedor.Velocidade {
				vencedor <- carro
			}
		}(i)
	}

	go func() {
		wg.Wait()
		close(canalCarros)
		close(vencedor)
	}()

	for carro := range canalCarros {
		fmt.Printf("Carro %d iniciou a corrida com velocidade %d\n", carro.ID, carro.Velocidade)
		time.Sleep(time.Duration(rand.Intn(5)) * time.Second)
		fmt.Printf("Carro %d terminou a corrida!\n", carro.ID)
	}

	time.Sleep(1 * time.Second)
}
```

Neste código, começamos definindo uma estrutura `Carro` que representa as informações de um carro, incluindo um ID e a velocidade do carro. 

Em seguida, no `main()`, definimos o número de carros `numCarros` e inicializamos um canal `canalCarros` para enviar os carros para a corrida. Também criamos um canal `vencedor` para receber o carro vencedor da corrida.

Criamos uma goroutine que recebe o carro vencedor do canal `vencedor` e imprime uma mensagem indicando qual carro venceu a corrida.

Em seguida, iniciamos um loop para criar goroutines para cada carro. Cada goroutine cria um carro com um ID e velocidade aleatórios e envia-o para o canal `canalCarros`. Se a velocidade do carro atual for maior do que a velocidade do vencedor atual, o carro é enviado para o canal `vencedor`.

Também criamos uma goroutine que espera até que todas as goroutines dos carros sejam concluídas usando a sincronização `sync.WaitGroup`. Após todas as goroutines terminarem, fechamos os canais `canalCarros` e `vencedor`.

Em seguida, percorremos o canal `canalCarros` usando um loop `for range` e imprimimos mensagens indicando quando cada carro inicia e termina a corrida. Usamos a função `time.Sleep` para simular a duração da corrida.

Finalmente, esperamos um segundo para permitir que as goroutines restantes terminem antes de encerrar o programa.

Esse código cria uma simulação de corrida de carros com vários competidores, onde cada carro é executado em uma goroutine separada e a velocidade é aleatória. O carro mais rápido é determinado usando canais e é impresso como o vencedor da corrida.