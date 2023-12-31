Claro! Aqui está um exemplo de um código em Go que implementa um algoritmo de busca binária recursiva em um vetor ordenado:

```go
package main

import (
	"fmt"
)

func buscaBinariaRecursiva(vetor []int, alvo, esquerda, direita int) int {
	if esquerda > direita {
		return -1
	}

	meio := (esquerda + direita) / 2

	if vetor[meio] == alvo {
		return meio
	} else if vetor[meio] > alvo {
		return buscaBinariaRecursiva(vetor, alvo, esquerda, meio-1)
	} else {
		return buscaBinariaRecursiva(vetor, alvo, meio+1, direita)
	}
}

func main() {
	vetor := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	alvo := 6

	resultado := buscaBinariaRecursiva(vetor, alvo, 0, len(vetor)-1)

	if resultado != -1 {
		fmt.Printf("O elemento %d foi encontrado no índice %d\n", alvo, resultado)
	} else {
		fmt.Printf("O elemento %d não foi encontrado no vetor\n", alvo)
	}
}
```

Neste código, primeiro importamos o pacote "fmt" para permitir a impressão dos resultados.

A função `buscaBinariaRecursiva` recebe como parâmetros um vetor ordenado (`vetor`), o valor alvo a ser buscado (`alvo`), e os índices esquerda (`esquerda`) e direita (`direita`) que determinam a faixa de busca atual. Inicialmente, verificamos se a faixa de busca é válida (caso `esquerda > direita`), retornando -1 para indicar que o elemento não foi encontrado.

Em seguida, calculamos o índice do elemento do meio da faixa de busca. Comparamos o valor do elemento do meio com o valor alvo e, caso sejam iguais, retornamos o índice do meio como resultado.

Caso o valor do elemento do meio seja maior que o valor alvo, chamamos a função `buscaBinariaRecursiva` para buscar na metade esquerda do vetor, atualizando a faixa de busca para a esquerda até o índice `meio-1`.

Caso contrário, chamamos a função `buscaBinariaRecursiva` para buscar na metade direita do vetor, atualizando a faixa de busca para a direita a partir do índice `meio+1`.

No `main()`, definimos um vetor de exemplo (`vetor := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}`) e um valor alvo a ser buscado (`alvo := 6`). Chamamos a função `buscaBinariaRecursiva` com os parâmetros apropriados e armazenamos o resultado na variável `resultado`.

Por fim, verificamos se o resultado é diferente de -1 (indicando que o elemento foi encontrado) e imprimimos a mensagem correspondente. Caso contrário, imprimimos uma mensagem informando que o elemento não foi encontrado no vetor.