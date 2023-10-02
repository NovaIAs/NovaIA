Claro! Aqui está um código complexo em Go que envolve manipulação de dados, estruturas de controle e funções:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	numeros := []float64{4, 9, 16, 25, 36, 49, 64, 81, 100} // Slice de números

	// Função para calcular a média dos números
	media := calcularMedia(numeros)
	fmt.Printf("A média dos números é: %.2f\n", media)

	// Função para calcular o desvio padrão dos números
	desvioPadrao := calcularDesvioPadrao(numeros)
	fmt.Printf("O desvio padrão dos números é: %.2f\n", desvioPadrao)

	// Função para encontrar o maior número
	maiorNumero := encontrarMaiorNumero(numeros)
	fmt.Printf("O maior número é: %.2f\n", maiorNumero)

	// Função para encontrar o menor número
	menorNumero := encontrarMenorNumero(numeros)
	fmt.Printf("O menor número é: %.2f\n", menorNumero)
}

func calcularMedia(numeros []float64) float64 {
	total := 0.0
	for _, num := range numeros {
		total += num
	}
	return total / float64(len(numeros))
}

func calcularDesvioPadrao(numeros []float64) float64 {
	media := calcularMedia(numeros)
	var somaDosQuadrados float64
	for _, num := range numeros {
		somaDosQuadrados += math.Pow(num-media, 2)
	}
	variancia := somaDosQuadrados / float64(len(numeros))
	return math.Sqrt(variancia)
}

func encontrarMaiorNumero(numeros []float64) float64 {
	maior := numeros[0]
	for _, num := range numeros {
		if num > maior {
			maior = num
		}
	}
	return maior
}

func encontrarMenorNumero(numeros []float64) float64 {
	menor := numeros[0]
	for _, num := range numeros {
		if num < menor {
			menor = num
		}
	}
	return menor
}
```

Esse código utiliza uma slice de números para realizar diferentes cálculos estatísticos. Ele calcula a média, o desvio padrão, encontra o maior e o menor número da lista. Os resultados são exibidos no console.