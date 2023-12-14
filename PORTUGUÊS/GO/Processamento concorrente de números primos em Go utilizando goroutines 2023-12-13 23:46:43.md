Aqui está um exemplo de um código complexo em Go que utiliza goroutines para processar uma lista de números primos de forma concorrente.

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	numbers := []int{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97}
	results := make(chan int)
	var wg sync.WaitGroup

	for _, num := range numbers {
		wg.Add(1)
		go checkPrime(num, results, &wg)
	}

	go func() {
		wg.Wait()
		close(results)
	}()

	for prime := range results {
		fmt.Printf("%d é primo\n", prime)
	}
}

func checkPrime(num int, results chan<- int, wg *sync.WaitGroup) {
	defer wg.Done()

	if num < 2 {
		return
	}

	isPrime := true
	for i := 2; i*i <= num; i++ {
		if num%i == 0 {
			isPrime = false
			break
		}
	}

	if isPrime {
		results <- num
	}
}
```

Neste código, temos uma função `checkPrime` que verifica se um número é primo ou não. Utilizamos a estrutura de repetição `for` para iterar de 2 até a raiz quadrada do número e, se encontrarmos algum divisor, o número não é primo. Caso contrário, o número é considerado primo e é enviado para o canal `results`.

Na função `main`, criamos uma lista de números primos e um canal `results` para receber os números primos encontrados pelas goroutines. Utilizamos a estrutura de repetição `for` para percorrer a lista de números e, para cada número, iniciamos uma goroutine que chama a função `checkPrime` para verificar se o número é primo ou não. Utilizamos também a variável `wg` do tipo `sync.WaitGroup` para aguardar a conclusão de todas as goroutines antes de fechar o canal `results`.

Por fim, utilizamos uma goroutine anônima para aguardar a conclusão de todas as goroutines através da função `Wait` da variável `wg` e, em seguida, fechamos o canal `results`. Utilizamos um loop `for` para imprimir os números primos recebidos do canal `results`.

Esse código utiliza concorrência para verificar os números primos de forma mais rápida, já que cada número é verificado em uma goroutine separada.