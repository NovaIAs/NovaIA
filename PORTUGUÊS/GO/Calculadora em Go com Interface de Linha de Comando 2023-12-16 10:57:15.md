Claro! Aqui está um código complexo em Go que envolve a criação de uma calculadora com interface de linha de comando.

```go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	reader := bufio.NewReader(os.Stdin)

	for {
		fmt.Println("Calculadora")
		fmt.Println("--------------------")
		fmt.Println("Selecione uma operação:")
		fmt.Println("1. Soma")
		fmt.Println("2. Subtração")
		fmt.Println("3. Multiplicação")
		fmt.Println("4. Divisão")
		fmt.Println("0. Sair")
		fmt.Print("Opção: ")
		option, _ := reader.ReadString('\n')
		option = strings.TrimSpace(option)

		switch option {
		case "1":
			fmt.Println("Digite os dois números separados por espaço:")
			numbers, _ := reader.ReadString('\n')
			numbers = strings.TrimSpace(numbers)
			numberArray := strings.Split(numbers, " ")
			if len(numberArray) != 2 {
				fmt.Println("Entrada inválida")
				continue
			}
			num1, _ := strconv.ParseFloat(numberArray[0], 64)
			num2, _ := strconv.ParseFloat(numberArray[1], 64)
			result := num1 + num2
			fmt.Println("Resultado:", result)
		case "2":
			fmt.Println("Digite os dois números separados por espaço:")
			numbers, _ := reader.ReadString('\n')
			numbers = strings.TrimSpace(numbers)
			numberArray := strings.Split(numbers, " ")
			if len(numberArray) != 2 {
				fmt.Println("Entrada inválida")
				continue
			}
			num1, _ := strconv.ParseFloat(numberArray[0], 64)
			num2, _ := strconv.ParseFloat(numberArray[1], 64)
			result := num1 - num2
			fmt.Println("Resultado:", result)
		case "3":
			fmt.Println("Digite os dois números separados por espaço:")
			numbers, _ := reader.ReadString('\n')
			numbers = strings.TrimSpace(numbers)
			numberArray := strings.Split(numbers, " ")
			if len(numberArray) != 2 {
				fmt.Println("Entrada inválida")
				continue
			}
			num1, _ := strconv.ParseFloat(numberArray[0], 64)
			num2, _ := strconv.ParseFloat(numberArray[1], 64)
			result := num1 * num2
			fmt.Println("Resultado:", result)
		case "4":
			fmt.Println("Digite os dois números separados por espaço:")
			numbers, _ := reader.ReadString('\n')
			numbers = strings.TrimSpace(numbers)
			numberArray := strings.Split(numbers, " ")
			if len(numberArray) != 2 {
				fmt.Println("Entrada inválida")
				continue
			}
			num1, _ := strconv.ParseFloat(numberArray[0], 64)
			num2, _ := strconv.ParseFloat(numberArray[1], 64)
			if num2 == 0 {
				fmt.Println("Não é possível dividir por zero")
				continue
			}
			result := num1 / num2
			fmt.Println("Resultado:", result)
		case "0":
			fmt.Println("Encerrando a calculadora...")
			return
		default:
			fmt.Println("Opção inválida")
		}
	}
}
```

Este código em Go implementa uma calculadora simples com as operações de soma, subtração, multiplicação e divisão. Ele utiliza a função `bufio.NewReader` para ler as entradas do usuário e `strconv.ParseFloat` para converter as strings dos números em valores float64.

O programa exibe um menu com as opções disponíveis e solicita ao usuário que escolha uma delas. Em seguida, ele pede ao usuário que digite dois números separados por espaço. O código verifica se a quantidade de números é válida e faz a conversão para float64. Dependendo da opção escolhida, o programa realiza a operação matemática correspondente e exibe o resultado na tela.

O loop principal garante que o programa continue a rodar até que o usuário escolha a opção "0" para sair. Caso o usuário digite uma opção inválida, o programa informa que a opção é inválida e solicita uma nova entrada.

Espero que esse código atenda às suas expectativas!