Claro! Aqui está um exemplo de código complexo em Go, que envolve a utilização de estruturas de dados avançadas e algoritmos:

```
package main

import (
	"fmt"
)

// Função para calcular a soma dos elementos de uma lista
func somaLista(lista []int) int {
	total := 0
	for _, num := range lista {
		total += num
	}
	return total
}

// Função para encontrar o maior elemento de uma lista
func encontrarMaior(lista []int) int {
	maior := lista[0]
	for _, num := range lista {
		if num > maior {
			maior = num
		}
	}
	return maior
}

// Função para encontrar o menor elemento de uma lista
func encontrarMenor(lista []int) int {
	menor := lista[0]
	for _, num := range lista {
		if num < menor {
			menor = num
		}
	}
	return menor
}

// Função para ordenar uma lista em ordem crescente usando o algoritmo Bubble Sort
func ordenarLista(lista []int) []int {
	tamanho := len(lista)
	for i := 0; i < tamanho-1; i++ {
		for j := 0; j < tamanho-i-1; j++ {
			if lista[j] > lista[j+1] {
				lista[j], lista[j+1] = lista[j+1], lista[j]
			}
		}
	}
	return lista
}

func main() {
	numeros := []int{9, 4, 7, 2, 1, 5, 8, 3, 6}

	// Calcula a soma dos elementos da lista
	soma := somaLista(numeros)
	fmt.Println("A soma dos elementos da lista é:", soma)

	// Encontra o maior elemento da lista
	maior := encontrarMaior(numeros)
	fmt.Println("O maior elemento da lista é:", maior)

	// Encontra o menor elemento da lista
	menor := encontrarMenor(numeros)
	fmt.Println("O menor elemento da lista é:", menor)

	// Ordena a lista em ordem crescente
	listaOrdenada := ordenarLista(numeros)
	fmt.Println("A lista ordenada em ordem crescente é:", listaOrdenada)
}
```

Neste código, começamos definindo algumas funções úteis para manipular listas de números. A função `somaLista` calcula a soma de todos os elementos de uma lista. A função `encontrarMaior` encontra o maior elemento da lista. A função `encontrarMenor` encontra o menor elemento da lista. E, finalmente, a função `ordenarLista` ordena a lista em ordem crescente usando o algoritmo Bubble Sort.

No `main`, criamos uma lista de números desordenada e chamamos as funções que calculam a soma, encontram o maior e o menor elemento, e ordenam a lista. Os resultados são impressos na saída padrão usando `fmt.Println`.

Este código ilustra o uso de estruturas de controle (loops), estruturas de dados (slices), passagem de parâmetros e retorno de valores em Go.