Certamente! Aqui está um exemplo de código complexo em Go que implementa um algoritmo de recomendação de filmes baseado em filtragem colaborativa:

```go
package main

import (
	"fmt"
	"math"
)

type Filme struct {
	Titulo       string
	Avaliacoes   map[string]float64
}

type Recomendacao struct {
	Filme     Filme
	Distancia float64
}

func calcularDistanciaEuclidiana(avaliacoesA, avaliacoesB map[string]float64) float64 {
	somaQuadrados := 0.0

	for filme, avaliacaoA := range avaliacoesA {
		if avaliacaoB, ok := avaliacoesB[filme]; ok {
			somaQuadrados += math.Pow(avaliacaoA-avaliacaoB, 2)
		}
	}

	return 1 / (1 + math.Sqrt(somaQuadrados))
}

func recomendarFilmes(avaliacoes map[string]map[string]float64, usuario string) []Recomendacao {
	recomendacoes := make([]Recomendacao, 0)

	avaliacoesUsuario := avaliacoes[usuario]

	for outroUsuario, avaliacoesOutroUsuario := range avaliacoes {
		if outroUsuario != usuario {
			distancia := calcularDistanciaEuclidiana(avaliacoesUsuario, avaliacoesOutroUsuario)

			for filme, avaliacao := range avaliacoesOutroUsuario {
				if _, ok := avaliacoesUsuario[filme]; !ok {
					recomendacoes = append(recomendacoes, Recomendacao{
						Filme:     Filme{Titulo: filme, Avaliacoes: map[string]float64{outroUsuario: avaliacao}},
						Distancia: distancia,
					})
				}
			}
		}
	}

	return recomendacoes
}

func main() {
	avaliacoes := map[string]map[string]float64{
		"Alice": {
			"Matrix":  5,
			"Inception": 4,
			"Interstellar": 3,
		},
		"Bob": {
			"Matrix":  4,
			"Inception": 5,
			"Interstellar": 2,
		},
		"Charlie": {
			"Matrix":  3,
			"Inception": 2,
			"Interstellar": 5,
		},
		"David": {
			"Matrix":  2,
			"Inception": 3,
			"Interstellar": 4,
		},
	}

	usuario := "Alice"

	fmt.Printf("Recomendações para %s:\n", usuario)
	recomendacoes := recomendarFilmes(avaliacoes, usuario)
	for _, recomendacao := range recomendacoes {
		fmt.Printf("Filme: %s, Distância: %f\n", recomendacao.Filme.Titulo, recomendacao.Distancia)
	}
}
```

Nesse exemplo, começamos definindo duas estruturas de dados: `Filme`, que representa um filme com seu título e avaliações por usuários, e `Recomendacao`, que contém o filme recomendado e a distância entre os usuários.

A função `calcularDistanciaEuclidiana` calcula a distância euclidiana entre dois usuários com base em suas avaliações. Quanto menor a distância, maior é a similaridade entre os usuários.

A função `recomendarFilmes` recebe as avaliações de todos os usuários e o nome do usuário para o qual queremos gerar recomendações. Ela calcula a distância entre o usuário de referência e todos os outros usuários, e verifica quais filmes ainda não foram avaliados pelo usuário de referência. As recomendações são armazenadas em um slice de `Recomendacao`, contendo o filme recomendado e a distância.

Na função `main`, definimos um mapa `avaliacoes` contendo as avaliações de alguns usuários para diferentes filmes. Em seguida, chamamos a função `recomendarFilmes` para gerar recomendações para o usuário "Alice". Por fim, imprimimos as recomendações obtidas.

Esse código implementa um algoritmo básico de recomendação de filmes, mas é claro que existem muitas outras técnicas e refinamentos que podem ser aplicados.