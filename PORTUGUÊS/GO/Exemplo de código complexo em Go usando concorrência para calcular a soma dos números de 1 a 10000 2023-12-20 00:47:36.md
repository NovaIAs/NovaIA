Claro! Aqui está um exemplo de código complexo em Go que usa concorrência para calcular a soma dos números de 1 a 10000:

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	var wg sync.WaitGroup
	numbers := make(chan int)
	result := make(chan int)
	go produceNumbers(numbers, 10000)
	go calculateSum(numbers, result)
	wg.Add(1)
	go printResult(result, &wg)

	wg.Wait()
}

func produceNumbers(numbers chan<- int, n int) {
	defer close(numbers)
	for i := 1; i <= n; i++ {
		numbers <- i
	}
}

func calculateSum(numbers <-chan int, result chan<- int) {
	sum := 0
	for num := range numbers {
		sum += num
	}
	result <- sum
	close(result)
}

func printResult(result <-chan int, wg *sync.WaitGroup) {
	defer wg.Done()
	sum := <-result
	fmt.Println("A soma dos números de 1 a 10000 é:", sum)
}
```

Explicação do código:
1. Importamos os pacotes "fmt" e "sync" para impressão e sincronização, respectivamente.
2. Definimos a função `main` como ponto de entrada do programa.
3. Criamos dois canais: `numbers` para produzir os números e `result` para receber o resultado.
4. Iniciamos duas goroutines usando as palavras-chave `go`:
  - `produceNumbers` gera os números de 1 a 10000 e envia para o canal `numbers`.
  - `calculateSum` consome os números do canal `numbers` e calcula a soma.
5. Usamos a variável `wg` do tipo `sync.WaitGroup` para aguardar a conclusão da goroutine `printResult`.
6. Iniciamos outra goroutine `printResult` para imprimir o resultado final.
7. Chamamos o método `Wait` da variável `wg` para aguardar a conclusão da goroutine.
8. A função `produceNumbers` preenche o canal `numbers` com os números de 1 a 10000 e, em seguida, o fecha.
9. A função `calculateSum` consome os números do canal `numbers`, soma-os e envia o resultado para o canal `result`, que é então fechado.
10. A função `printResult` recebe o resultado do canal `result` e imprime na tela.

Esse código utiliza concorrência para gerar os números, calcular a soma e imprimir o resultado de forma assíncrona, melhorando o desempenho do programa.