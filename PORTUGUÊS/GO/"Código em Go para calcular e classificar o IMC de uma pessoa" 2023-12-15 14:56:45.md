Claro! Aqui está um exemplo de um código complexo em Go que envolve uma estrutura de dados personalizada e operações aritméticas:

```go
package main

import (
	"fmt"
	"math"
)

type Pessoa struct {
	nome      string
	idade     int
	altura    float64
	peso      float64
	imc       float64
	classificacao string
}

func main() {
	// Criando uma instância da estrutura Pessoa
	pessoa := Pessoa{
		nome:   "João",
		idade:  25,
		altura: 1.75,
		peso:   70,
	}

	// Calculando o IMC (índice de massa corporal)
	pessoa.imc = calcularIMC(pessoa.peso, pessoa.altura)

	// Classificando a pessoa com base no IMC
	pessoa.classificacao = classificarIMC(pessoa.imc)

	// Exibindo os dados da pessoa
	fmt.Println("Dados da Pessoa:")
	fmt.Println("Nome:", pessoa.nome)
	fmt.Println("Idade:", pessoa.idade)
	fmt.Println("Altura:", pessoa.altura)
	fmt.Println("Peso:", pessoa.peso)
	fmt.Println("IMC:", pessoa.imc)
	fmt.Println("Classificação:", pessoa.classificacao)
}

func calcularIMC(peso, altura float64) float64 {
	return peso / math.Pow(altura, 2)
}

func classificarIMC(imc float64) string {
	if imc < 18.5 {
		return "Abaixo do peso"
	} else if imc < 24.9 {
		return "Peso normal"
	} else if imc < 29.9 {
		return "Sobrepeso"
	} else {
		return "Obesidade"
	}
}
```

Neste código em Go, criamos uma estrutura de dados chamada "Pessoa" que armazena informações como nome, idade, altura, peso, IMC e classificação do IMC. Em seguida, temos a função principal "main" onde criamos uma instância da estrutura "Pessoa". Calculamos o IMC usando a função "calcularIMC" e classificamos a pessoa usando a função "classificarIMC". Por fim, exibimos os dados da pessoa na saída do programa.

Espero que este exemplo de código atenda às suas expectativas!