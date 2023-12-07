Claro! Aqui está um exemplo de código complexo em Go:

```go
package main

import (
	"fmt"
	"math"
)

// Função que verifica se um número é primo
func ehPrimo(num int) bool {
	if num < 2 {
		return false
	}
	for i := 2; i <= int(math.Sqrt(float64(num))); i++ {
		if num%i == 0 {
			return false
		}
	}
	return true
}

// Função que retorna a sequência de Fibonacci até um determinado limite
func fibonacci(limite int) []int {
	fib := []int{0, 1}
	for i := 2; ; i++ {
		fib = append(fib, fib[i-1]+fib[i-2])
		if fib[i] >= limite {
			break
		}
	}
	return fib
}

// Função principal
func main() {
	// Encontrar e imprimir todos os números primos até 100000
	primeirosPrimos := []int{}
	for i := 2; i <= 100000; i++ {
		if ehPrimo(i) {
			primeirosPrimos = append(primeirosPrimos, i)
		}
	}
	fmt.Println("Números primos até 100000:", primeirosPrimos)

	// Calcular e imprimir os 20 primeiros números da sequência de Fibonacci
	fibonacci20 := fibonacci(20)
	fmt.Println("Primeiros 20 números da sequência de Fibonacci:", fibonacci20)

	// Calcular e imprimir a soma dos quadrados dos números primos até 10000
	somaQuadrados := 0
	for _, primo := range primeirosPrimos {
		if primo < 10000 {
			somaQuadrados += primo * primo
		}
	}
	fmt.Println("Soma dos quadrados dos números primos até 10000:", somaQuadrados)
}
```

Neste código, o programa encontra e imprime todos os números primos até 100000, calcula e imprime os 20 primeiros números da sequência de Fibonacci e, por fim, calcula e imprime a soma dos quadrados dos números primos até 10000.

A função `ehPrimo(num int) bool` recebe um número inteiro como argumento e retorna verdadeiro se o número for primo ou falso caso contrário. Essa função utiliza um loop for para verificar se o número é divisível por qualquer número entre 2 e a raiz quadrada do próprio número, evitando verificar divisões desnecessárias.

A função `fibonacci(limite int) []int` retorna uma fatia (slice) contendo a sequência de Fibonacci até um determinado limite. A sequência é construída adicionando o próximo número como a soma dos dois números anteriores, até que o próximo número seja maior ou igual ao limite fornecido.

A função principal `main` utiliza as funções `ehPrimo` e `fibonacci` para realizar os cálculos e imprimir os resultados na tela.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, por favor, me avise.