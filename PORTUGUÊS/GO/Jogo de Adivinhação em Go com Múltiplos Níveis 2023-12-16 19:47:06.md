Claro! Aqui está um código complexo em Go que envolve a criação de um jogo de adivinhação com múltiplos níveis. 

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())

	fmt.Println("Bem-vindo ao Jogo de Adivinhação!")
	fmt.Println("Escolha um nível de dificuldade:")
	fmt.Println("1 - Fácil (números entre 1 e 10)")
	fmt.Println("2 - Médio (números entre 1 e 50)")
	fmt.Println("3 - Difícil (números entre 1 e 100)")

	var nivel int
	fmt.Scanln(&nivel)

	var maximo int
	switch nivel {
	case 1:
		maximo = 10
	case 2:
		maximo = 50
	case 3:
		maximo = 100
	default:
		fmt.Println("Nível inválido. Reinicie o jogo.")
		return
	}

	numeroSecreto := rand.Intn(maximo) + 1
	var tentativas int

	for {
		fmt.Println("Adivinhe o número:")
		var palpite int
		fmt.Scanln(&palpite)

		tentativas++

		if palpite < numeroSecreto {
			fmt.Println("Palpite baixo. Tente novamente!")
		} else if palpite > numeroSecreto {
			fmt.Println("Palpite alto. Tente novamente!")
		} else {
			fmt.Printf("Parabéns! Você acertou o número em %d tentativas.\n", tentativas)
			break
		}
	}

	fmt.Println("Obrigado por jogar!")
}
```

Neste código, começamos importando os pacotes necessários, como "fmt" para lidar com a entrada e saída, "math/rand" para gerar números aleatórios e "time" para definir a semente do gerador aleatório.

Em seguida, exibimos uma mensagem de boas-vindas e solicitamos ao jogador que escolha um nível de dificuldade. Dependendo do nível escolhido, definimos um valor máximo para o número secreto.

Em seguida, geramos o número secreto aleatório usando a função `rand.Intn(maximo) + 1`, onde `maximo` é o valor máximo definido anteriormente.

Em um loop infinito, solicitamos ao jogador que adivinhe o número. Comparamos o palpite do jogador com o número secreto e fornecemos dicas (palpite alto ou baixo) até que o jogador adivinhe corretamente.

Quando o jogador acerta o número, exibimos uma mensagem parabenizando-o e mostrando o número de tentativas feitas. Em seguida, encerramos o jogo.

Espero que este código atenda às suas expectativas!