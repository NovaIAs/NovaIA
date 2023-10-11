Claro! Aqui está um exemplo de código complexo em Go:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	numeros := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

	fmt.Println("Números originais:", numeros)

	// Quadrado dos números
	fmt.Println("Quadrado dos números:")
	for i := 0; i < len(numeros); i++ {
		numeros[i] = numeros[i] * numeros[i]
		fmt.Println(numeros[i])
	}

	// Soma dos números
	soma := 0
	for _, num := range numeros {
		soma += num
	}
	fmt.Println("Soma dos números:", soma)

	// Média dos números
	media := float64(soma) / float64(len(numeros))
	fmt.Println("Média dos números:", media)

	// Raiz quadrada da soma dos números
	raiz := math.Sqrt(float64(soma))
	fmt.Println("Raiz quadrada da soma dos números:", raiz)

	// Números pares e ímpares
	pares := []int{}
	impares := []int{}
	for _, num := range numeros {
		if num%2 == 0 {
			pares = append(pares, num)
		} else {
			impares = append(impares, num)
		}
	}
	fmt.Println("Números pares:", pares)
	fmt.Println("Números ímpares:", impares)
}
```

Neste exemplo de código em Go, começamos com uma lista de números e realizamos várias operações com eles. Primeiro, calculamos o quadrado de cada número e os exibimos na tela. Em seguida, somamos todos os números e mostramos o resultado. Calculamos a média dos números e a raiz quadrada da soma. Por fim, separamos os números em pares e ímpares e os exibimos separadamente.