package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())

	// Variáveis para armazenar as informações dos funcionários
	quantidadeFuncionarios := 10
	nomesFuncionarios := []string{"João", "Maria", "José", "Ana", "Pedro", "Mariana", "Carlos", "Laura", "Rafael", "Luiza"}
	idadesFuncionarios := make([]int, quantidadeFuncionarios)
	salariosFuncionarios := make([]float64, quantidadeFuncionarios)

	// Preenche as idades e salários dos funcionários aleatoriamente
	for i := 0; i < quantidadeFuncionarios; i++ {
		idadesFuncionarios[i] = rand.Intn(30) + 20
		salariosFuncionarios[i] = float64(rand.Intn(1000) + 1000)
	}

	// Calcula a média de idade dos funcionários
	mediaIdade := calcularMedia(idadesFuncionarios)

	// Calcula o salário total da empresa
	salarioTotal := calcularTotal(salariosFuncionarios)

	// Imprime as informações dos funcionários
	for i := 0; i < quantidadeFuncionarios; i++ {
		fmt.Printf("Funcionário: %s\n", nomesFuncionarios[i])
		fmt.Printf("Idade: %d\n", idadesFuncionarios[i])
		fmt.Printf("Salário: R$ %.2f\n", salariosFuncionarios[i])
		fmt.Println("----------------------------------")
	}

	fmt.Printf("Média de idade: %.2f\n", mediaIdade)
	fmt.Printf("Salário total da empresa: R$ %.2f\n", salarioTotal)
}

func calcularMedia(valores []int) float64 {
	soma := 0

	for _, valor := range valores {
		soma += valor
	}

	return float64(soma) / float64(len(valores))
}

func calcularTotal(valores []float64) float64 {
	soma := 0.0

	for _, valor := range valores {
		soma += valor
	}

	return soma
}

Nesse código em Go, criamos um programa que simula uma empresa com 10 funcionários. Cada funcionário possui um nome, idade e salário.

A primeira parte é a declaração das variáveis: quantidadeFuncionarios é o número total de funcionários, nomesFuncionarios é uma lista com os nomes dos funcionários, idadesFuncionarios é um slice para armazenar as idades e salariosFuncionarios é um slice para armazenar os salários.

Em seguida, usamos um loop para preencher as idades e salários dos funcionários de forma aleatória. Usamos a função rand.Intn para gerar números aleatórios e atribuímos às variáveis idadesFuncionarios e salariosFuncionarios.

Depois disso, temos duas funções auxiliares: calcularMedia e calcularTotal. A função calcularMedia recebe um slice de inteiros e retorna a média dos valores. A função calcularTotal recebe um slice de float64 e retorna a soma dos valores.

Por fim, imprimimos as informações dos funcionários usando um loop. Para cada funcionário, imprimimos o nome, idade e salário. Também calculamos a média de idade e o salário total da empresa e imprimimos esses valores.

Esse código é um exemplo de como manipular e calcular informações de um conjunto de funcionários em uma empresa. Claro que, em uma aplicação real, poderíamos ter estruturas de dados mais complexas e funcionalidades adicionais, mas esse código básico já demonstra o uso de slices, loops e funções em Go.