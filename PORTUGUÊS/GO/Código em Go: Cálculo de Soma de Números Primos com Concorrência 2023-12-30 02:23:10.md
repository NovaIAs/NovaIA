Claro! Segue abaixo um código complexo em Go que utiliza concorrência para calcular a soma de todos os números primos em um intervalo específico. O código faz uso da técnica conhecida como "Crivo de Eratóstenes" para determinar quais números são primos.

package main

import (
	"fmt"
	"math"
	"sync"
)

func main() {
	var inicio, fim int

	fmt.Print("Informe o início do intervalo: ")
	fmt.Scan(&inicio)

	fmt.Print("Informe o fim do intervalo: ")
	fmt.Scan(&fim)

	primos := calcularPrimos(inicio, fim)

	total := calcularSomaPrimos(primos)

	fmt.Printf("A soma dos números primos no intervalo de %d a %d é: %d\n", inicio, fim, total)
}

func calcularPrimos(inicio, fim int) []int {
	var wg sync.WaitGroup
	var mutex sync.Mutex

	numeros := make([]int, fim-inicio+1)
	primos := []int{}

	for i := range numeros {
		numeros[i] = inicio + i
	}

	for _, num := range numeros {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			if ehPrimo(n) {
				mutex.Lock()
				primos = append(primos, n)
				mutex.Unlock()
			}
		}(num)
	}

	wg.Wait()

	return primos
}

func ehPrimo(n int) bool {
	if n <= 1 {
		return false
	}

	for i := 2; i <= int(math.Sqrt(float64(n))); i++ {
		if n%i == 0 {
			return false
		}
	}

	return true
}

func calcularSomaPrimos(primos []int) int {
	total := 0

	for _, num := range primos {
		total += num
	}

	return total
}

Neste código, o usuário informa o início e o fim do intervalo desejado. A função `calcularPrimos` é responsável por chamar a função `ehPrimo` de forma concorrente para verificar quais números são primos dentro do intervalo. Esses números primos são armazenados em um slice chamado `primos`.

A função `ehPrimo` verifica se um número é primo utilizando o algoritmo do "Crivo de Eratóstenes". Ela percorre todos os números de 2 até a raiz quadrada do número e verifica se algum deles é divisor do número em questão. Caso seja encontrado um divisor, o número não é primo e a função retorna `false`. Caso contrário, o número é primo e a função retorna `true`.

Por fim, a função `calcularSomaPrimos` é responsável por calcular a soma de todos os números primos presentes no slice `primos`.

Espero que este código atenda às suas expectativas!