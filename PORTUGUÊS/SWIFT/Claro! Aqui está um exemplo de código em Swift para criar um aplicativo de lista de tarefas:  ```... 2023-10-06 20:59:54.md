Claro! Aqui está um exemplo de código complexo em Swift para criar um aplicativo de lista de tarefas com recursos avançados:

```swift
import UIKit

// Definição da estrutura de uma tarefa
struct Task {
    var title: String
    var description: String
    var date: Date
    var completed: Bool
}

// Classe principal para o aplicativo de lista de tarefas
class ToDoListApp {
    var tasks: [Task] = []
    
    // Adiciona uma nova tarefa à lista
    func addTask(title: String, description: String, date: Date) {
        let newTask = Task(title: title, description: description, date: date, completed: false)
        tasks.append(newTask)
    }
    
    // Marca uma tarefa como concluída
    func completeTask(at index: Int) {
        if index >= 0 && index < tasks.count {
            tasks[index].completed = true
        }
    }
    
    // Lista todas as tarefas pendentes
    func listPendingTasks() {
        let pendingTasks = tasks.filter { !$0.completed }
        
        if pendingTasks.isEmpty {
            print("Não há tarefas pendentes.")
        } else {
            print("Tarefas pendentes:")
            for (index, task) in pendingTasks.enumerated() {
                print("\(index + 1). \(task.title) - \(task.description)")
            }
        }
    }
    
    // Lista todas as tarefas concluídas
    func listCompletedTasks() {
        let completedTasks = tasks.filter { $0.completed }
        
        if completedTasks.isEmpty {
            print("Não há tarefas concluídas.")
        } else {
            print("Tarefas concluídas:")
            for (index, task) in completedTasks.enumerated() {
                print("\(index + 1). \(task.title) - \(task.description)")
            }
        }
    }
}

// Exemplo de uso do aplicativo de lista de tarefas
let app = ToDoListApp()

app.addTask(title: "Comprar mantimentos", description: "Ir ao supermercado e comprar os itens da lista", date: Date())
app.addTask(title: "Estudar programação", description: "Revisar conceitos de programação orientada a objetos", date: Date())
app.addTask(title: "Fazer exercícios físicos", description: "Correr por 30 minutos na esteira", date: Date())

app.completeTask(at: 0)

app.listPendingTasks()
app.listCompletedTasks()
```

Nesse código, temos a definição da estrutura `Task`, que representa uma tarefa com título, descrição, data e status de conclusão. A classe `ToDoListApp` é responsável por gerenciar a lista de tarefas. Ela possui métodos para adicionar tarefas, marcar uma tarefa como concluída e listar as tarefas pendentes e concluídas.

No exemplo de uso do aplicativo, são criadas três tarefas, sendo uma delas marcada como concluída. Em seguida, as tarefas pendentes e concluídas são listadas.

Esse código exemplifica um aplicativo de lista de tarefas simples, mas com recursos avançados, como a marcação de tarefas como concluídas e a listagem separada de tarefas pendentes e concluídas. É importante notar que esse é apenas um exemplo e o código pode ser adaptado e expandido de acordo com as necessidades do projeto.