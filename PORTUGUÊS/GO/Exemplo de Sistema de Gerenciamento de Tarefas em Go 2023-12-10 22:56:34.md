Claro! Abaixo está um exemplo de um código complexo em Go que implementa um sistema de gerenciamento de tarefas.

```go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Tarefa struct {
	ID          int
	Titulo      string
	Descricao   string
	Concluida   bool
	Responsavel string
}

type GerenciadorTarefas struct {
	Tarefas []Tarefa
}

func (gt *GerenciadorTarefas) adicionarTarefa(t Tarefa) {
	gt.Tarefas = append(gt.Tarefas, t)
	fmt.Println("Tarefa adicionada com sucesso!")
}

func (gt *GerenciadorTarefas) listarTarefas() {
	fmt.Println("Lista de Tarefas:")
	for _, tarefa := range gt.Tarefas {
		concluida := "Não"
		if tarefa.Concluida {
			concluida = "Sim"
		}
		fmt.Printf("ID: %d\nTítulo: %s\nDescrição: %s\nConcluída: %s\nResponsável: %s\n\n", tarefa.ID, tarefa.Titulo, tarefa.Descricao, concluida, tarefa.Responsavel)
	}
}

func (gt *GerenciadorTarefas) concluirTarefa(id int) {
	for i, tarefa := range gt.Tarefas {
		if tarefa.ID == id {
			gt.Tarefas[i].Concluida = true
			fmt.Println("Tarefa concluída com sucesso!")
			return
		}
	}
	fmt.Println("Tarefa não encontrada.")
}

func main() {
	gt := GerenciadorTarefas{}

	scanner := bufio.NewScanner(os.Stdin)

	for {
		fmt.Println("----- MENU -----")
		fmt.Println("1 - Adicionar Tarefa")
		fmt.Println("2 - Listar Tarefas")
		fmt.Println("3 - Concluir Tarefa")
		fmt.Println("0 - Sair")
		fmt.Print("Escolha uma opção: ")

		scanner.Scan()
		opcao, _ := strconv.Atoi(scanner.Text())

		switch opcao {
		case 1:
			fmt.Print("Digite o título da tarefa: ")
			scanner.Scan()
			titulo := scanner.Text()

			fmt.Print("Digite a descrição da tarefa: ")
			scanner.Scan()
			descricao := scanner.Text()

			fmt.Print("Digite o responsável pela tarefa: ")
			scanner.Scan()
			responsavel := scanner.Text()

			t := Tarefa{
				ID:          len(gt.Tarefas) + 1,
				Titulo:      titulo,
				Descricao:   descricao,
				Concluida:   false,
				Responsavel: responsavel,
			}

			gt.adicionarTarefa(t)

		case 2:
			gt.listarTarefas()

		case 3:
			fmt.Print("Digite o ID da tarefa que deseja concluir: ")
			scanner.Scan()
			id, _ := strconv.Atoi(scanner.Text())

			gt.concluirTarefa(id)

		case 0:
			fmt.Println("Saindo do programa...")
			os.Exit(0)

		default:
			fmt.Println("Opção inválida.")
		}
	}
}
```

Este código implementa um sistema simples de gerenciamento de tarefas. Ele possui uma estrutura `Tarefa` que representa uma tarefa e suas propriedades, como ID, título, descrição, status de conclusão e responsável.

Além disso, há uma estrutura `GerenciadorTarefas` que contém uma lista de tarefas e métodos para adicionar tarefas, listar tarefas e concluir tarefas.

No `main()`, há um loop que exibe um menu com opções para adicionar tarefas, listar tarefas, concluir tarefas e sair do programa. O usuário pode escolher a opção desejada digitando o número correspondente.

Cada opção é tratada através de um `switch` e o código solicita ao usuário as informações necessárias para adicionar uma tarefa, listar as tarefas ou concluir uma tarefa.

Espero que esse exemplo atenda às suas expectativas e demonstre a complexidade e o diferencial do código em Go.