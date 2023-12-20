Claro! Aqui está um código complexo em Go:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Gerando uma semente aleatória baseada no tempo atual
	rand.Seed(time.Now().UnixNano())

	// Criando uma função para gerar um número aleatório entre 1 e 100
	randomNumber := func() int {
		return rand.Intn(100) + 1
	}

	// Criando uma função para verificar se um número é primo
	isPrime := func(num int) bool {
		if num < 2 {
			return false
		}
		for i := 2; i*i <= num; i++ {
			if num%i == 0 {
				return false
			}
		}
		return true
	}

	// Gerando um número aleatório
	num := randomNumber()

	fmt.Printf("Número gerado: %d\n", num)

	// Verificando se o número é primo
	if isPrime(num) {
		fmt.Println("O número é primo!")
	} else {
		fmt.Println("O número não é primo!")
	}
}
```

Neste código em Go, criamos uma função `randomNumber` que irá gerar um número aleatório entre 1 e 100. Em seguida, temos a função `isPrime` que verifica se um número é primo ou não.

No `main()`, geramos um número aleatório utilizando a função `randomNumber` e o exibimos na tela. Em seguida, utilizamos a função `isPrime` para verificar se o número gerado é primo ou não, e exibimos a mensagem apropriada.

Espero que este código complexo em Go atenda às suas expectativas!