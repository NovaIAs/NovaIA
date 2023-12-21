Claro! Aqui está um código complexo em Go que proporciona uma experiência interativa para o usuário. Ele permite que o usuário jogue um jogo de adivinhação onde deve tentar acertar um número aleatório gerado pelo programa. O código está comentado para facilitar a compreensão:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Gera uma semente única para o gerador de números aleatórios
	rand.Seed(time.Now().UnixNano())

	// Gera um número aleatório entre 1 e 100
	numeroAleatorio := rand.Intn(100) + 1

	// Variável para armazenar o palpite do usuário
	var palpite int

	// Contador de tentativas
	tentativas := 0

	fmt.Println("Bem-vindo ao Jogo de Adivinhação!")

	for {
		// Solicita um palpite ao usuário
		fmt.Print("Digite um número entre 1 e 100: ")
		fmt.Scanf("%d", &palpite)

		// Incrementa o contador de tentativas
		tentativas++

		// Verifica se o palpite está correto
		if palpite == numeroAleatorio {
			fmt.Printf("Parabéns! Você acertou o número em %d tentativas.\n", tentativas)
			break
		} else if palpite > numeroAleatorio {
			fmt.Println("Tente um número menor.")
		} else {
			fmt.Println("Tente um número maior.")
		}
	}

	fmt.Println("Obrigado por jogar!")
}
```

Neste código, a função `main` é o ponto de partida do programa. O jogo começa gerando um número aleatório entre 1 e 100 usando a função `rand.Intn`. Em seguida, o programa entra num loop infinito onde solicita ao usuário que faça um palpite. O palpite é comparado com o número aleatório e feedback é fornecido ao usuário sobre se o palpite foi maior ou menor do que o número a ser adivinhado.

O loop continua até que o usuário acerte o número, momento em que o programa exibe uma mensagem de parabéns e informa o número de tentativas feitas. Após o usuário acertar, o loop é interrompido e o programa exibe uma mensagem de agradecimento.

Espero que este código atenda às suas expectativas! Se você precisar de mais ajuda ou tiver alguma dúvida, fique à vontade para perguntar.