Claro! Aqui está um código complexo em Go para criar uma calculadora interativa com operações básicas:

```go
package main

import (
	"fmt"
	"strconv"
)

func main() {
	for {
		operacao := menu()

		switch operacao {
		case 1:
			operacaoSoma()
		case 2:
			operacaoSubtracao()
		case 3:
			operacaoMultiplicacao()
		case 4:
			operacaoDivisao()
		case 5:
			fmt.Println("Saindo...")
			return
		default:
			fmt.Println("Opção inválida. Tente novamente.")
		}
	}
}

func menu() int {
	fmt.Println("----- Calculadora -----")
	fmt.Println("Selecione uma operação:")
	fmt.Println("1 - Soma")
	fmt.Println("2 - Subtração")
	fmt.Println("3 - Multiplicação")
	fmt.Println("4 - Divisão")
	fmt.Println("5 - Sair")

	var opcao int
	fmt.Print("Opção: ")
	fmt.Scan(&opcao)

	return opcao
}

func operacaoSoma() {
	fmt.Println("----- Soma -----")
	num1, num2 := lerNumeros()

	resultado := num1 + num2
	fmt.Println("Resultado:", resultado)
}

func operacaoSubtracao() {
	fmt.Println("----- Subtração -----")
	num1, num2 := lerNumeros()

	resultado := num1 - num2
	fmt.Println("Resultado:", resultado)
}

func operacaoMultiplicacao() {
	fmt.Println("----- Multiplicação -----")
	num1, num2 := lerNumeros()

	resultado := num1 * num2
	fmt.Println("Resultado:", resultado)
}

func operacaoDivisao() {
	fmt.Println("----- Divisão -----")
	num1, num2 := lerNumeros()

	if num2 == 0 {
		fmt.Println("Não é possível dividir por zero.")
		return
	}

	resultado := num1 / num2
	fmt.Println("Resultado:", resultado)
}

func lerNumeros() (float64, float64) {
	var num1, num2 float64

	fmt.Print("Digite o primeiro número: ")
	fmt.Scan(&num1)

	fmt.Print("Digite o segundo número: ")
	fmt.Scan(&num2)

	return num1, num2
}
```

Explicação do código:

1. Primeiro, importamos os pacotes necessários: `fmt` para a entrada/saída e `strconv` para conversão de string para número.
2. Em seguida, definimos a função `main()`, que será o ponto de entrada do programa.
3. Dentro da função `main()`, temos um loop infinito `for` que continuará executando até que o usuário escolha a opção de sair.
4. A função `menu()` é chamada para exibir as opções disponíveis ao usuário e ler a opção escolhida.
5. Com base na opção escolhida, utilizamos uma estrutura `switch` para direcionar o fluxo do programa para a função correspondente.
6. Cada função de operação (soma, subtração, multiplicação e divisão) solicita ao usuário os números necessários e realiza a operação correspondente.
7. A função `lerNumeros()` é utilizada para ler dois números do usuário e retorná-los como valores individuais.
8. Ao final de cada operação, o resultado é exibido na tela.
9. Quando o usuário escolhe a opção de sair, uma mensagem é exibida e o programa é encerrado.