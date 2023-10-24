Claro! Aqui está um exemplo de um código complexo em Go:

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Define o tamanho do slice e gera números aleatórios
    tamanho := 10
    numeros := gerarNumerosAleatorios(tamanho)

    // Imprime os números gerados aleatoriamente
    fmt.Println("Números gerados:", numeros)

    // Ordena os números em ordem crescente usando o algoritmo Bubble Sort
    bubbleSort(numeros)

    // Imprime os números ordenados
    fmt.Println("Números ordenados:", numeros)
}

func gerarNumerosAleatorios(tamanho int) []int {
    numeros := make([]int, tamanho)

    // Gera uma semente aleatória usando o tempo atual
    rand.Seed(time.Now().UnixNano())

    // Preenche o slice com números aleatórios entre 1 e 100
    for i := 0; i < tamanho; i++ {
        numeros[i] = rand.Intn(100) + 1
    }

    return numeros
}

func bubbleSort(numeros []int) {
    tamanho := len(numeros)

    for i := 0; i < tamanho-1; i++ {
        for j := 0; j < tamanho-i-1; j++ {
            // Compara dois elementos adjacentes e os troca se estiverem na ordem errada
            if numeros[j] > numeros[j+1] {
                numeros[j], numeros[j+1] = numeros[j+1], numeros[j]
            }
        }
    }
}
```

Este código em Go é um exemplo de um programa que gera uma lista de números aleatórios e os ordena usando o algoritmo Bubble Sort. 

A função `gerarNumerosAleatorios` cria um slice de tamanho definido e preenche-o com números aleatórios entre 1 e 100, utilizando a função `rand.Intn`. A semente para a geração dos números aleatórios é definida usando o tempo atual.

A função `bubbleSort` implementa o algoritmo Bubble Sort para ordenar os números. Ele compara elementos adjacentes e os troca de posição se estiverem na ordem errada. O processo é repetido várias vezes até que a lista esteja completamente ordenada.

O programa principal `main` chama as funções `gerarNumerosAleatorios` e `bubbleSort`, e imprime os números gerados aleatoriamente e os números ordenados usando a função `fmt.Println`.

Espero que este exemplo complexo em Go tenha atendido às suas expectativas! Se houver mais alguma coisa em que eu possa ajudar, por favor, avise-me.