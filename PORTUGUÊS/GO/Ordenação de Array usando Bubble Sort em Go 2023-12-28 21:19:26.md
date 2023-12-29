Claro! Aqui está um código complexo em Go que realiza a ordenação de um array de números usando o algoritmo Bubble Sort.

```go
package main

import "fmt"

func bubbleSort(arr []int) {
	n := len(arr)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			if arr[j] > arr[j+1] {
				arr[j], arr[j+1] = arr[j+1], arr[j]
			}
		}
	}
}

func main() {
	arr := []int{64, 34, 25, 12, 22, 11, 90}
	fmt.Println("Array antes da ordenação:", arr)
	bubbleSort(arr)
	fmt.Println("Array após a ordenação:", arr)
}
```

Neste código, a função `bubbleSort` implementa o algoritmo de ordenação Bubble Sort. O algoritmo percorre o array várias vezes, comparando elementos adjacentes e os trocando de lugar se estiverem fora de ordem. Esse processo é repetido até que o array esteja completamente ordenado.

A função `main` cria um array de exemplo com alguns números desordenados e, em seguida, chama a função `bubbleSort` para ordená-lo. Por fim, imprime o array antes e depois da ordenação.

Ao executar este código, você verá a saída:

```
Array antes da ordenação: [64 34 25 12 22 11 90]
Array após a ordenação: [11 12 22 25 34 64 90]
```

Espero que este código atenda às suas expectativas!