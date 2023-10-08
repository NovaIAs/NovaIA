Claro! Aqui está um código complexo em Go que cria uma aplicação de lista de tarefas com funcionalidades avançadas. Vou explicar cada parte do código para que você possa entender melhor sua estrutura e funcionalidade.

```go
package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

type Task struct {
	ID       int
	Title    string
	Complete bool
}

type TaskList struct {
	Tasks []Task
}

func clearScreen() {
	cmd := exec.Command("clear") // Use "cls" ao invés de "clear" para sistemas Windows
	cmd.Stdout = os.Stdout
	cmd.Run()
}

func printTaskList(taskList TaskList) {
	fmt.Println("Tarefas:")
	fmt.Println("------------------------")
	for _, task := range taskList.Tasks {
		completeStatus := "[ ]"
		if task.Complete {
			completeStatus = "[X]"
		}
		fmt.Printf("%d. %s %s\n", task.ID, completeStatus, task.Title)
	}
	fmt.Println("------------------------")
}

func addTask(taskList *TaskList, title string) {
	taskID := len(taskList.Tasks) + 1
	task := Task{
		ID:       taskID,
		Title:    title,
		Complete: false,
	}
	taskList.Tasks = append(taskList.Tasks, task)
	fmt.Printf("Tarefa '%s' adicionada com sucesso!\n", title)
}

func completeTask(taskList *TaskList, taskID int) {
	for i, task := range taskList.Tasks {
		if task.ID == taskID {
			taskList.Tasks[i].Complete = true
			fmt.Printf("Tarefa '%s' marcada como concluída!\n", task.Title)
			return
		}
	}
	fmt.Printf("Tarefa com ID %d não encontrada!\n", taskID)
}

func deleteTask(taskList *TaskList, taskID int) {
	for i, task := range taskList.Tasks {
		if task.ID == taskID {
			taskList.Tasks = append(taskList.Tasks[:i], taskList.Tasks[i+1:]...)
			fmt.Printf("Tarefa '%s' removida com sucesso!\n", task.Title)
			return
		}
	}
	fmt.Printf("Tarefa com ID %d não encontrada!\n", taskID)
}

func main() {
	taskList := TaskList{}
	for {
		clearScreen()
		printTaskList(taskList)
		fmt.Println("------------------------")
		fmt.Println("Selecione uma opção:")
		fmt.Println("1. Adicionar Tarefa")
		fmt.Println("2. Concluir Tarefa")
		fmt.Println("3. Remover Tarefa")
		fmt.Println("0. Sair")
		fmt.Println("------------------------")
		var option int
		fmt.Print("Opção: ")
		fmt.Scanf("%d", &option)
		switch option {
		case 1:
			fmt.Print("Digite o título da tarefa: ")
			var title string
			fmt.Scanf("%s", &title)
			addTask(&taskList, title)
		case 2:
			fmt.Print("Digite o ID da tarefa a ser concluída: ")
			var taskID int
			fmt.Scanf("%d", &taskID)
			completeTask(&taskList, taskID)
		case 3:
			fmt.Print("Digite o ID da tarefa a ser removida: ")
			var taskID int
			fmt.Scanf("%d", &taskID)
			deleteTask(&taskList, taskID)
		case 0:
			fmt.Println("Encerrando a aplicação...")
			os.Exit(0)
		default:
			fmt.Println("Opção inválida!")
		}
		fmt.Print("Pressione Enter para continuar...")
		fmt.Scanln()
	}
}
```

Este código é uma aplicação de linha de comando que permite gerenciar uma lista de tarefas. Ele usa uma estrutura de dados `Task` para representar cada tarefa, contendo um ID, um título e um campo `Complete` para indicar se a tarefa foi concluída ou não.

A estrutura de dados `TaskList` é usada para armazenar uma lista de tarefas. Os métodos `addTask`, `completeTask` e `deleteTask` são usados para adicionar, concluir e excluir tarefas da lista, respectivamente.

A função `clearScreen` é usada para limpar a tela do console antes de exibir a lista de tarefas. Isso torna a interface mais amigável para o usuário.

A função `printTaskList` é responsável por imprimir a lista de tarefas atualizada no console. Ela itera sobre cada tarefa e imprime seu ID, estado de conclusão e título.

No `main`, o programa entra em um loop infinito onde exibe o menu de opções para o usuário e aguarda a entrada. Com base na escolha do usuário, o programa chama a função correspondente para adicionar, concluir ou excluir tarefas.

Espero que este código complexo em Go atenda às suas expectativas! Se tiver mais alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.