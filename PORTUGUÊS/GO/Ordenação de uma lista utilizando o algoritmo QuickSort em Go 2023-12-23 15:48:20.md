Claro! Aqui está um código complexo em Go que realiza a ordenação de uma lista de números utilizando o algoritmo de ordenação QuickSort:

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	lista := gerarListaAleatoria(10)
	fmt.Println("Lista original:", lista)

	quickSort(lista, 0, len(lista)-1)
	fmt.Println("Lista ordenada:", lista)
}

func gerarListaAleatoria(tamanho int) []int {
	lista := make([]int, tamanho)
	rand.Seed(time.Now().UnixNano())

	for i := 0; i < tamanho; i++ {
		lista[i] = rand.Intn(100)
	}

	return lista
}

func quickSort(lista []int, inicio int, fim int) {
	if inicio < fim {
		posicaoPivo := particionar(lista, inicio, fim)
		quickSort(lista, inicio, posicaoPivo-1)
		quickSort(lista, posicaoPivo+1, fim)
	}
}

func particionar(lista []int, inicio int, fim int) int {
	pivo := lista[fim]
	i := inicio - 1

	for j := inicio; j < fim; j++ {
		if lista[j] < pivo {
			i++
			lista[i], lista[j] = lista[j], lista[i]
		}
	}

	lista[i+1], lista[fim] = lista[fim], lista[i+1]
	return i + 1
}
```

Neste código, a função `main` é o ponto de entrada do programa. Ela gera uma lista de 10 números aleatórios utilizando a função `gerarListaAleatoria`, e em seguida, chama a função `quickSort` para ordenar a lista.

A função `gerarListaAleatoria` cria uma lista com o tamanho especificado como parâmetro e preenche-a com números aleatórios entre 0 e 100.

A função `quickSort` implementa o algoritmo de ordenação QuickSort. Ela recebe a lista, o índice de início e fim como parâmetros. A função seleciona um elemento como pivô, particiona a lista ao redor do pivô e recursivamente aplica o QuickSort nas duas sublistas resultantes.

A função `particionar` é auxiliar ao QuickSort e realiza a partição da lista. Ela seleciona o último elemento da lista como pivô, percorre a lista da esquerda para a direita e, caso encontre um elemento menor que o pivô, realiza a troca. Ao final, coloca o pivô em sua posição correta e retorna essa posição.

No final, o programa exibe a lista original e a lista ordenada.

Espero que esse código atenda às suas expectativas!