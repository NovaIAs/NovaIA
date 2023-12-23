Claro! Aqui está um exemplo de código em Go que implementa uma simulação de um sistema de gerenciamento de funcionários em uma empresa:

```go
package main

import (
	"fmt"
	"strconv"
)

type Funcionario struct {
	nome       string
	sobrenome  string
	idade      int
	salario    float64
	departamento string
}

type Empresa struct {
	funcionarios []Funcionario
}

func (e *Empresa) adicionarFuncionario(funcionario Funcionario) {
	e.funcionarios = append(e.funcionarios, funcionario)
}

func (e *Empresa) exibirFuncionarios() {
	fmt.Println("Funcionários da Empresa:")
	for _, funcionario := range e.funcionarios {
		fmt.Printf("Nome: %s %s\n", funcionario.nome, funcionario.sobrenome)
		fmt.Printf("Idade: %d\n", funcionario.idade)
		fmt.Printf("Salário: R$%.2f\n", funcionario.salario)
		fmt.Printf("Departamento: %s\n\n", funcionario.departamento)
	}
}

func main() {
	empresa := Empresa{}

	// Adicionar funcionários
	empresa.adicionarFuncionario(Funcionario{
		nome:       "João",
		sobrenome:  "Silva",
		idade:      25,
		salario:    5000.00,
		departamento: "TI",
	})

	empresa.adicionarFuncionario(Funcionario{
		nome:       "Maria",
		sobrenome:  "Santos",
		idade:      30,
		salario:    7000.00,
		departamento: "Vendas",
	})

	empresa.adicionarFuncionario(Funcionario{
		nome:       "Pedro",
		sobrenome:  "Gomes",
		idade:      35,
		salario:    6000.00,
		departamento: "RH",
	})

	// Exibir funcionários
	empresa.exibirFuncionarios()
}
```

Neste código, definimos duas structs: `Funcionario` e `Empresa`. A struct `Funcionario` representa as informações de um funcionário, como nome, sobrenome, idade, salário e departamento. A struct `Empresa` possui um slice de `Funcionario`, que representa a lista de funcionários da empresa.

Em seguida, temos dois métodos associados à struct `Empresa`: `adicionarFuncionario` e `exibirFuncionarios`. O método `adicionarFuncionario` recebe um `Funcionario` como parâmetro e adiciona o funcionário à lista de funcionários da empresa. O método `exibirFuncionarios` percorre a lista de funcionários e exibe suas informações na saída padrão.

No `main`, criamos uma instância da struct `Empresa` e adicionamos três funcionários. Em seguida, chamamos o método `exibirFuncionarios` para mostrar a lista de funcionários na saída.

Esse código simula um sistema básico de gerenciamento de funcionários em uma empresa, permitindo adicionar novos funcionários e exibir a lista de funcionários existentes. Você pode expandir esse código adicionando novos recursos, como remover funcionários, buscar funcionários por departamento, etc.