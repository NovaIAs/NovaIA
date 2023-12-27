Claro! Aqui está um código complexo em Go, que utiliza concorrência para realizar operações matemáticas em paralelo. O código é dividido em várias etapas para facilitar a explicação:

```go
package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

func main() {
	// Gerando números aleatórios
	numbers := generateRandomNumbers(100)

	// Dividindo os números em 4 partes
	chunks := splitIntoChunks(numbers, 4)

	// Criando um canal para receber os resultados
	resultChan := make(chan int)

	// Criando um WaitGroup para sincronizar as goroutines
	var wg sync.WaitGroup

	// Iniciando 4 goroutines para calcular a soma de cada parte
	for _, chunk := range chunks {
		wg.Add(1)
		go calculateSum(chunk, &wg, resultChan)
	}

	// Utilizando outra goroutine para esperar todas as goroutines terminarem
	go func() {
		wg.Wait()
		close(resultChan)
	}()

	// Recebendo os resultados do canal e calculando a soma total
	totalSum := 0
	for result := range resultChan {
		totalSum += result
	}

	// Imprimindo o resultado
	fmt.Println("A soma total é:", totalSum)
}

// Função para gerar números aleatórios
func generateRandomNumbers(n int) []int {
	rand.Seed(time.Now().UnixNano())

	numbers := make([]int, n)
	for i := 0; i < n; i++ {
		numbers[i] = rand.Intn(100)
	}

	return numbers
}

// Função para dividir um slice de números em partes iguais
func splitIntoChunks(numbers []int, numChunks int) [][]int {
	chunkSize := (len(numbers) + numChunks - 1) / numChunks
	chunks := make([][]int, numChunks)

	for i := 0; i < numChunks; i++ {
		start := i * chunkSize
		end := start + chunkSize

		if end > len(numbers) {
			end = len(numbers)
		}

		chunks[i] = numbers[start:end]
	}

	return chunks
}

// Função para calcular a soma de um slice de números e enviar o resultado pelo canal
func calculateSum(numbers []int, wg *sync.WaitGroup, resultChan chan<- int) {
	defer wg.Done()

	sum := 0
	for _, num := range numbers {
		sum += num
	}

	resultChan <- sum
}
```

Explicação do código:
1. Começamos importando os pacotes necessários para o nosso código, como "fmt" para imprimir resultados, "math/rand" para gerar números aleatórios, "sync" para utilizar o WaitGroup e "time" para gerar uma semente aleatória.

2. Na função `main`, geramos uma lista de 100 números aleatórios através da função `generateRandomNumbers`.

3. Em seguida, dividimos essa lista em 4 partes iguais utilizando a função `splitIntoChunks`.

4. Criamos um canal chamado `resultChan` para receber os resultados das goroutines que serão criadas.

5. Criamos um WaitGroup chamado `wg` para sincronizar as goroutines.

6. Iniciamos 4 goroutines, uma para cada parte da lista de números, utilizando um loop `for`. Para cada parte, chamamos a função `calculateSum` em uma goroutine separada, passando o chunk de números correspondente, o ponteiro para o WaitGroup `wg` e o canal `resultChan`.

7. Utilizamos outra goroutine para aguardar todas as goroutines terminarem, utilizando um função anônima. Dentro dessa função, chamamos o método `Wait` do WaitGroup `wg` para esperar até que todas as goroutines sejam concluídas, e então fechamos o canal `resultChan` para indicar que não haverá mais envio de resultados.

8. Em seguida, utilizamos um loop `for` para receber os resultados do canal `resultChan` e calcular a soma total. A variável `totalSum` é inicializada como 0 e, em cada iteração do loop, recebemos um valor do canal e o adicionamos à variável `totalSum`.

9. Por fim, imprimimos o resultado da soma total.

Essa implementação aproveita a concorrência para realizar a soma dos números em paralelo, dividindo a lista em partes menores que podem ser processadas simultaneamente por goroutines separadas. O WaitGroup é utilizado para garantir que todas as goroutines sejam concluídas antes de calcular a soma final.