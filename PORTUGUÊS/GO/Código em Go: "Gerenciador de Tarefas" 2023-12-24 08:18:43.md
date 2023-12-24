Claro! Vou criar um código complexo em Go para você. Este código será um programa de gerenciamento de tarefas chamado "Task Manager". Ele permitirá que os usuários criem, visualizem, atualizem e excluam tarefas. O código será organizado em diferentes pacotes para manter a modularidade e a legibilidade.

Vamos começar com o pacote "main", que será o ponto de entrada do programa:

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	taskManager := NewTaskManager()

	for {
		menu()
		option := getUserOption()

		switch option {
		case 1:
			taskManager.createTask()
		case 2:
			taskManager.viewTasks()
		case 3:
			taskManager.updateTask()
		case 4:
			taskManager.deleteTask()
		case 5:
			os.Exit(0)
		default:
			fmt.Println("Opção inválida. Por favor, tente novamente.")
		}
	}
}

func menu() {
	fmt.Println("==== Task Manager ====")
	fmt.Println("1 - Criar tarefa")
	fmt.Println("2 - Visualizar tarefas")
	fmt.Println("3 - Atualizar tarefa")
	fmt.Println("4 - Excluir tarefa")
	fmt.Println("5 - Sair")
}

func getUserOption() int {
	var option int
	fmt.Print("Opção: ")
	fmt.Scanln(&option)
	return option
}
```

Agora, vamos criar o pacote "task" para lidar com as operações de tarefa:

```go
package task

import (
	"fmt"
)

type Task struct {
	ID       int
	Title    string
	Deadline string
	Status   string
}

type TaskManager struct {
	tasks []Task
}

func NewTaskManager() *TaskManager {
	return &TaskManager{}
}

func (tm *TaskManager) createTask() {
	var task Task
	fmt.Print("Título da tarefa: ")
	fmt.Scanln(&task.Title)
	fmt.Print("Prazo da tarefa: ")
	fmt.Scanln(&task.Deadline)
	task.ID = len(tm.tasks) + 1
	task.Status = "Pendente"
	tm.tasks = append(tm.tasks, task)
	fmt.Println("Tarefa criada com sucesso!")
}

func (tm *TaskManager) viewTasks() {
	if len(tm.tasks) == 0 {
		fmt.Println("Não há tarefas para exibir.")
		return
	}

	fmt.Println("==== Tarefas ====")
	for _, task := range tm.tasks {
		fmt.Printf("ID: %d\n", task.ID)
		fmt.Printf("Título: %s\n", task.Title)
		fmt.Printf("Prazo: %s\n", task.Deadline)
		fmt.Printf("Status: %s\n\n", task.Status)
	}
}

func (tm *TaskManager) updateTask() {
	if len(tm.tasks) == 0 {
		fmt.Println("Não há tarefas para atualizar.")
		return
	}

	fmt.Print("ID da tarefa a ser atualizada: ")
	var taskID int
	fmt.Scanln(&taskID)

	for index, task := range tm.tasks {
		if task.ID == taskID {
			fmt.Printf("Nova descrição da tarefa (atual: %s): ", task.Title)
			fmt.Scanln(&task.Title)
			fmt.Printf("Novo prazo da tarefa (atual: %s): ", task.Deadline)
			fmt.Scanln(&task.Deadline)
			tm.tasks[index] = task
			fmt.Println("Tarefa atualizada com sucesso!")
			return
		}
	}

	fmt.Println("Tarefa não encontrada.")
}

func (tm *TaskManager) deleteTask() {
	if len(tm.tasks) == 0 {
		fmt.Println("Não há tarefas para excluir.")
		return
	}

	fmt.Print("ID da tarefa a ser excluída: ")
	var taskID int
	fmt.Scanln(&taskID)

	for index, task := range tm.tasks {
		if task.ID == taskID {
			tm.tasks = append(tm.tasks[:index], tm.tasks[index+1:]...)
			fmt.Println("Tarefa excluída com sucesso!")
			return
		}
	}

	fmt.Println("Tarefa não encontrada.")
}
```

Agora podemos executar o programa e gerenciar tarefas usando o Task Manager. Espero que isso atenda às suas expectativas!