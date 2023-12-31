Claro! Aqui está um exemplo de um programa em Go que realiza uma pesquisa binária em uma lista ordenada:

```go
package main

import (
	"fmt"
)

func pesquisaBinaria(lista []int, valor int) int {
	esquerda := 0
	direita := len(lista) - 1

	for esquerda <= direita {
		meio := (esquerda + direita) / 2

		if lista[meio] == valor {
			return meio
		} else if lista[meio] < valor {
			esquerda = meio + 1
		} else {
			direita = meio - 1
		}
	}

	return -1
}

func main() {
	lista := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	valor := 6

	resultado := pesquisaBinaria(lista, valor)

	if resultado == -1 {
		fmt.Println("O valor", valor, "não foi encontrado na lista.")
	} else {
		fmt.Println("O valor", valor, "foi encontrado na posição", resultado, "da lista.")
	}
}
```

Explicação do código:

1. Começamos importando o pacote "fmt" para podermos imprimir mensagens no terminal.

2. Em seguida, definimos a função `pesquisaBinaria` que recebe uma lista de inteiros e um valor a ser pesquisado. Essa função retorna a posição do valor na lista (-1 se não encontrado).

3. Dentro da função `pesquisaBinaria`, inicializamos os índices `esquerda` e `direita` para determinar os limites da busca na lista.

4. Em um loop, enquanto `esquerda` for menor ou igual a `direita`, calculamos o índice do meio da lista utilizando a fórmula `(esquerda + direita) / 2`.

5. Comparamos o valor na posição `meio` com o valor que estamos buscando. Se forem iguais, retornamos a posição `meio`. Se o valor na posição `meio` for menor que o valor buscado, atualizamos `esquerda` para `meio + 1`, indicando que a busca deve continuar na metade direita da lista. Caso contrário, atualizamos `direita` para `meio - 1`, indicando que a busca deve continuar na metade esquerda da lista.

6. Se o valor não for encontrado no loop, retornamos -1.

7. Na função `main`, criamos uma lista ordenada de 1 a 10 e definimos o valor que queremos buscar como 6.

8. Chamamos a função `pesquisaBinaria` passando a lista e o valor, e armazenamos o resultado na variável `resultado`.

9. Por fim, verificamos se o resultado é -1, indicando que o valor não foi encontrado, e imprimimos a mensagem correspondente. Caso contrário, imprimimos a mensagem informando a posição em que o valor foi encontrado.