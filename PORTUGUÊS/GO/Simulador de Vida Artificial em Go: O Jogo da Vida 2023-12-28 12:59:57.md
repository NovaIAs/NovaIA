Claro! Aqui está um código em Go que implementa um simulador de vida artificial baseado em células. O código cria um tabuleiro bidimensional, onde cada célula pode estar viva ou morta. O objetivo é simular a evolução das células ao longo das gerações, seguindo as regras clássicas do "Jogo da Vida", formulado por John Conway.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

const (
	LarguraTabuleiro  = 50
	AlturaTabuleiro   = 20
	NumGeracoes       = 100
	ProbabilidadeVida = 0.3
)

type Celula struct {
	viva bool
}

func criarTabuleiro() [][]Celula {
	tabuleiro := make([][]Celula, AlturaTabuleiro)
	for i := range tabuleiro {
		tabuleiro[i] = make([]Celula, LarguraTabuleiro)
	}
	return tabuleiro
}

func inicializarTabuleiro(tabuleiro [][]Celula) {
	rand.Seed(time.Now().UnixNano())
	for i := 0; i < AlturaTabuleiro; i++ {
		for j := 0; j < LarguraTabuleiro; j++ {
			if rand.Float64() < ProbabilidadeVida {
				tabuleiro[i][j].viva = true
			}
		}
	}
}

func exibirTabuleiro(tabuleiro [][]Celula) {
	for i := 0; i < AlturaTabuleiro; i++ {
		for j := 0; j < LarguraTabuleiro; j++ {
			if tabuleiro[i][j].viva {
				fmt.Print("■ ")
			} else {
				fmt.Print("□ ")
			}
		}
		fmt.Println()
	}
}

func contarVizinhosVivos(tabuleiro [][]Celula, x, y int) int {
	count := 0

	coordenadasVizinhos := [][]int{
		{-1, -1}, {-1, 0}, {-1, 1},
		{0, -1}, /* (x, y) */ {0, 1},
		{1, -1}, {1, 0}, {1, 1},
	}

	for _, coordenada := range coordenadasVizinhos {
		vizinhoX := x + coordenada[0]
		vizinhoY := y + coordenada[1]

		if vizinhoX >= 0 && vizinhoX < AlturaTabuleiro && vizinhoY >= 0 && vizinhoY < LarguraTabuleiro {
			if tabuleiro[vizinhoX][vizinhoY].viva {
				count++
			}
		}
	}

	return count
}

func proximaGeracao(tabuleiro [][]Celula) [][]Celula {
	novoTabuleiro := criarTabuleiro()

	for i := 0; i < AlturaTabuleiro; i++ {
		for j := 0; j < LarguraTabuleiro; j++ {
			numVizinhosVivos := contarVizinhosVivos(tabuleiro, i, j)
			if tabuleiro[i][j].viva {
				// Regra 1: Qualquer célula viva com menos de dois vizinhos vivos morre de solidão.
				// Regra 3: Qualquer célula viva com mais de três vizinhos vivos morre de superpopulação.
				if numVizinhosVivos < 2 || numVizinhosVivos > 3 {
					novoTabuleiro[i][j].viva = false
				} else {
					novoTabuleiro[i][j].viva = true
				}
			} else {
				// Regra 4: Qualquer célula morta com exatamente três vizinhos vivos se torna uma célula viva.
				if numVizinhosVivos == 3 {
					novoTabuleiro[i][j].viva = true
				}
			}
		}
	}

	return novoTabuleiro
}

func main() {
	tabuleiro := criarTabuleiro()
	inicializarTabuleiro(tabuleiro)

	fmt.Println("Tabuleiro inicial:")
	exibirTabuleiro(tabuleiro)

	for geracao := 1; geracao <= NumGeracoes; geracao++ {
		fmt.Printf("\nGeracao %d:\n", geracao)
		tabuleiro = proximaGeracao(tabuleiro)
		exibirTabuleiro(tabuleiro)
		time.Sleep(500 * time.Millisecond)
	}
}
```

Este código implementa o "Jogo da Vida" utilizando uma matriz bidimensional para representar o tabuleiro. A função `criarTabuleiro` cria um tabuleiro vazio e a função `inicializarTabuleiro` preenche o tabuleiro com células vivas de forma aleatória, com base na probabilidade definida.

A função `exibirTabuleiro` imprime o estado atual do tabuleiro na tela, utilizando símbolos unicode para representar células vivas (■) e células mortas (□).

A função `contarVizinhosVivos` conta o número de células vizinhas vivas para uma célula específica, considerando as regras de vizinhança de 8 células.

A função `proximaGeracao` implementa as regras do "Jogo da Vida". Ela percorre cada célula do tabuleiro, verifica o número de vizinhos vivos e aplica as regras para determinar se a célula deve viver ou morrer na próxima geração.

No `main`, o código cria o tabuleiro, inicializa-o aleatoriamente e exibe o tabuleiro inicial. Em seguida, ele itera por um número de gerações definido em `NumGeracoes`, atualizando e exibindo o tabuleiro em cada iteração. Um pequeno atraso de 500 milissegundos é adicionado entre cada geração para criar uma animação suave.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais esclarecimentos, estou à disposição para ajudar.