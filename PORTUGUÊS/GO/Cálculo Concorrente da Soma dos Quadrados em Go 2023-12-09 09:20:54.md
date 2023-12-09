Claro! Aqui está um exemplo de código complexo em Go que usa concorrência para calcular a soma dos quadrados de um conjunto de números:

```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

func calcularQuadrado(n int) int {
	time.Sleep(time.Duration(rand.Intn(10)) * time.Millisecond)
	return n * n
}

func calcularSomaQuadrados(numeros []int) int {
	var wg sync.WaitGroup
	resultados := make(chan int, len(numeros))

	for _, n := range numeros {
		wg.Add(1)
		go func(x int) {
			resultados <- calcularQuadrado(x)
			wg.Done()
		}(n)
	}

	go func() {
		wg.Wait()
		close(resultados)
	}()

	soma := 0
	for r := range resultados {
		soma += r
	}

	return soma
}

func main() {
	numeros := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

	somaQuadrados := calcularSomaQuadrados(numeros)

	fmt.Println("A soma dos quadrados é:", somaQuadrados)
}
```

Neste código, temos uma função `calcularQuadrado` que recebe um número inteiro e retorna o quadrado desse número. Essa função é usada pela função `calcularSomaQuadrados`, que recebe um conjunto de números e calcula a soma dos quadrados de forma concorrente.

Na função `calcularSomaQuadrados`, usamos uma goroutine para calcular o quadrado de cada número do conjunto de forma paralela. Para isso, criamos um canal `resultados` para receber os resultados de cada cálculo. Utilizamos também uma WaitGroup `wg` para garantir que todas as goroutines sejam concluídas antes de prosseguirmos.

Dentro do loop, criamos uma goroutine anônima que recebe cada número do conjunto como parâmetro e envia o resultado do cálculo para o canal `resultados`. Ao final de cada goroutine, chamamos `wg.Done()` para sinalizar que a goroutine foi concluída.

Após iniciar as goroutines, chamamos `wg.Wait()` para aguardar a conclusão de todas elas e, em seguida, fechamos o canal `resultados` para indicar que não haverá mais envios.

Por fim, somamos todos os resultados do canal `resultados` e retornamos a soma.

No `main()`, definimos um conjunto de números e chamamos a função `calcularSomaQuadrados` para obter a soma dos quadrados. O resultado é então impresso na tela.