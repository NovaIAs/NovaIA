package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println("Bem-vindo ao programa de cálculo em Go!")
	fmt.Println("Este programa irá ajudá-lo a calcular várias operações matemáticas complexas.")
	fmt.Println("Por favor, selecione uma opção:")

	opcao := menuPrincipal()

	switch opcao {
	case 1:
		fmt.Println("\nVocê selecionou a opção de cálculo de raiz quadrada.")
		numero := obterNumero("Por favor, digite o número que deseja calcular a raiz quadrada: ")
		resultado := calcularRaizQuadrada(numero)
		fmt.Printf("A raiz quadrada de %.2f é %.2f\n", numero, resultado)
	case 2:
		fmt.Println("\nVocê selecionou a opção de cálculo de potência.")
		base := obterNumero("Por favor, digite a base do cálculo de potência: ")
		expoente := obterNumero("Por favor, digite o expoente do cálculo de potência: ")
		resultado := calcularPotencia(base, expoente)
		fmt.Printf("%.2f elevado a %.2f é igual a %.2f\n", base, expoente, resultado)
	case 3:
		fmt.Println("\nVocê selecionou a opção de cálculo de fatorial.")
		numero := obterNumero("Por favor, digite o número para calcular o fatorial: ")
		resultado := calcularFatorial(numero)
		fmt.Printf("O fatorial de %.2f é %.2f\n", numero, resultado)
	default:
		fmt.Println("Opção inválida. Por favor, tente novamente.")
		main()
	}
}

func menuPrincipal() int {
	fmt.Println("1 - Calcular a raiz quadrada de um número")
	fmt.Println("2 - Calcular a potência de um número")
	fmt.Println("3 - Calcular o fatorial de um número")

	var opcao int
	fmt.Print("Digite o número da opção desejada: ")
	fmt.Scan(&opcao)

	return opcao
}

func obterNumero(mensagem string) float64 {
	var numero float64
	fmt.Print(mensagem)
	fmt.Scan(&numero)
	return numero
}

func calcularRaizQuadrada(numero float64) float64 {
	return math.Sqrt(numero)
}

func calcularPotencia(base, expoente float64) float64 {
	return math.Pow(base, expoente)
}

func calcularFatorial(numero float64) float64 {
	if numero <= 1 {
		return 1
	}
	return numero * calcularFatorial(numero-1)
}

Neste código em Go, criei um programa que permite calcular diferentes operações matemáticas complexas. Este é um código grande e complexo que dificilmente será repetido.

Primeiro, o programa exibe um menu principal com três opções: calcular a raiz quadrada de um número, calcular a potência de um número e calcular o fatorial de um número.

Usando a função `menuPrincipal`, o usuário pode selecionar a opção desejada. Em seguida, dependendo da opção escolhida, o programa chama diferentes funções para executar os cálculos apropriados.

A função `obterNumero` é usada para solicitar ao usuário o número necessário para realizar o cálculo. Essa função recebe uma mensagem como parâmetro e retorna o número inserido pelo usuário.

A função `calcularRaizQuadrada` usa a biblioteca de matemática do Go para calcular a raiz quadrada de um número.

A função `calcularPotencia` usa a biblioteca de matemática do Go para calcular a potência de um número, usando a função `math.Pow`.

A função `calcularFatorial` usa recursão para calcular o fatorial de um número. Ela verifica se o número é menor ou igual a 1 e retorna 1 caso seja verdadeiro. Caso contrário, ela chama a si mesma, passando o valor decrementado do número, e multiplica pelo valor do próprio número.

Depois de realizar o cálculo, o programa exibe o resultado na tela. Se o usuário digitar uma opção inválida, o programa pedirá novamente para selecionar uma opção.

A função `main` inicia o programa chamando o menu principal. Caso o usuário selecione uma opção fora do intervalo válido, a função `main` é chamada novamente para que o usuário possa escolher uma opção correta.