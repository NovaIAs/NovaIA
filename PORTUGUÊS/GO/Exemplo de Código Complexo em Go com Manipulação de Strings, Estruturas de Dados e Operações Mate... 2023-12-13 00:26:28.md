Claro! Aqui está um exemplo de um código complexo em Go que envolve manipulação de strings, estruturas de dados e operações matemáticas:

```go
package main

import (
	"fmt"
	"math"
	"strings"
)

// Função para calcular a raiz cúbica de um número
func calcularRaizCubica(numero float64) float64 {
	return math.Cbrt(numero)
}

// Função para inverter uma string
func inverterString(texto string) string {
	runes := []rune(texto)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

// Estrutura de dados para armazenar informações de uma pessoa
type Pessoa struct {
	Nome     string
	Idade    int
	Profissao string
}

// Função para imprimir informações de uma pessoa
func imprimirInformacoesPessoa(pessoa Pessoa) {
	fmt.Printf("Nome: %s\n", pessoa.Nome)
	fmt.Printf("Idade: %d\n", pessoa.Idade)
	fmt.Printf("Profissão: %s\n", pessoa.Profissao)
}

// Função principal
func main() {
	// Exemplo de utilização das funções e estruturas de dados definidas acima

	// Cálculo da raiz cúbica de um número
	numero := 27.0
	raizCubica := calcularRaizCubica(numero)
	fmt.Printf("A raiz cúbica de %.2f é %.2f\n", numero, raizCubica)

	// Inversão de uma string
	texto := "Olá, mundo!"
	textoInvertido := inverterString(texto)
	fmt.Printf("A string \"%s\" invertida é: \"%s\"\n", texto, textoInvertido)

	// Criação e impressão das informações de uma pessoa
	pessoa := Pessoa{
		Nome:     "João",
		Idade:    30,
		Profissao: "Engenheiro",
	}
	imprimirInformacoesPessoa(pessoa)
}
```

Neste código, temos a definição de algumas funções e uma estrutura de dados. A função `calcularRaizCubica` calcula a raiz cúbica de um número usando a função `math.Cbrt` da biblioteca `math`. A função `inverterString` inverte uma string, percorrendo seus caracteres e trocando a posição deles. A estrutura de dados `Pessoa` é usada para armazenar informações de uma pessoa, como nome, idade e profissão. A função `imprimirInformacoesPessoa` recebe uma pessoa como parâmetro e imprime suas informações.

Na função `main`, são realizados exemplos de utilização das funções e estruturas definidas. É calculada a raiz cúbica de um número, invertida uma string e criada uma pessoa com informações específicas, que são então impressas na tela.