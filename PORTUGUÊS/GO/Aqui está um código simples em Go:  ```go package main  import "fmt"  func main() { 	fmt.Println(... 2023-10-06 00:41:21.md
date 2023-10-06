Claro! Aqui está um código complexo em Go:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Definindo uma função anônima para gerar um número aleatório entre 1 e 100
	generateRandomNumber := func() int {
		rand.Seed(time.Now().UnixNano())
		return rand.Intn(100) + 1
	}

	// Definindo uma função recursiva para calcular o fatorial de um número
	calculateFactorial := func(n int) int {
		if n == 0 {
			return 1
		}
		return n * calculateFactorial(n-1)
	}

	// Definindo uma função para verificar se um número é primo
	isPrime := func(n int) bool {
		if n <= 1 {
			return false
		}
		for i := 2; i*i <= n; i++ {
			if n%i == 0 {
				return false
			}
		}
		return true
	}

	// Gerando um número aleatório
	randomNumber := generateRandomNumber()

	// Calculando o fatorial do número gerado
	factorial := calculateFactorial(randomNumber)

	// Verificando se o fatorial do número é primo
	isFactorialPrime := isPrime(factorial)

	// Imprimindo os resultados
	fmt.Printf("Número gerado: %d\n", randomNumber)
	fmt.Printf("Fatorial do número gerado: %d\n", factorial)
	if isFactorialPrime {
		fmt.Println("O fatorial do número gerado é primo!")
	} else {
		fmt.Println("O fatorial do número gerado não é primo.")
	}
}
```

Neste código em Go, eu defini uma função anônima `generateRandomNumber` para gerar um número aleatório entre 1 e 100 usando a biblioteca `math/rand` e `time`. Em seguida, defini uma função recursiva `calculateFactorial` para calcular o fatorial de um número. Utilizei a recursão para obter o resultado de forma eficiente.

Além disso, implementei uma função `isPrime` para verificar se um número é primo ou não. Essa função utiliza um loop para verificar se o número é divisível por algum outro número menor que ele mesmo. Caso seja encontrado algum divisor, o número não é primo.

No corpo principal do código, gerei um número aleatório usando a função `generateRandomNumber`, calculei o fatorial desse número utilizando a função `calculateFactorial` e verifiquei se o fatorial é primo com a função `isPrime`. Por fim, imprimo os resultados na tela.

Espero que este código complexo em Go atenda às suas expectativas!