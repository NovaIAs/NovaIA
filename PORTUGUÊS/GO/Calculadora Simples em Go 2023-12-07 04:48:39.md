Claro! Aqui está um exemplo de um código em Go que cria uma calculadora simples de linha de comando:

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
	fmt.Println("Bem-vindo à Calculadora")
	for {
		fmt.Print("Digite uma expressão matemática (+, -, *, /) ou 'sair' para encerrar: ")
		input, _ := reader.ReadString('\n')
		input = strings.TrimSpace(input)

		if input == "sair" {
			fmt.Println("Encerrando a Calculadora...")
			break
		}

		expression := strings.Split(input, " ")
		if len(expression) != 3 {
			fmt.Println("Expressão inválida. Tente novamente.")
			continue
		}

		operator := expression[1]
		number1, err := strconv.ParseFloat(expression[0], 64)
		if err != nil {
			fmt.Println("Número inválido. Tente novamente.")
			continue
		}
		number2, err := strconv.ParseFloat(expression[2], 64)
		if err != nil {
			fmt.Println("Número inválido. Tente novamente.")
			continue
		}

		result := 0.0
		switch operator {
		case "+":
			result = number1 + number2
		case "-":
			result = number1 - number2
		case "*":
			result = number1 * number2
		case "/":
			if number2 == 0 {
				fmt.Println("Não é possível dividir por zero. Tente novamente.")
				continue
			}
			result = number1 / number2
		default:
			fmt.Println("Operador inválido. Tente novamente.")
			continue
		}

		fmt.Printf("Resultado: %.2f\n", result)
	}
}
```

Nesse código, começamos importando as bibliotecas padrão necessárias para a leitura de entrada do usuário. Em seguida, definimos a função `main()` como ponto de entrada do programa. 

Esse programa utiliza um loop infinito para pedir ao usuário uma expressão matemática (+, -, *, /) ou a palavra "sair" para encerrar o programa. A expressão é analisada e validada para garantir que esteja correta. Em seguida, os números são extraídos da expressão e convertidos em tipo `float64`. 

Dependendo do operador, a calculadora realiza a operação correspondente e exibe o resultado. Caso ocorra algum erro durante a validação ou cálculo, uma mensagem de erro é exibida e o usuário é solicitado a fornecer uma nova expressão.

O programa continua pedindo expressões matemáticas até o usuário digitar "sair", momento em que o programa é encerrado.

Espero que este exemplo tenha sido útil! Fique à vontade para adaptar e estender o código conforme suas necessidades.