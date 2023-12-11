Aqui está um exemplo de um código complexo em Swift que envolve o desenvolvimento de um aplicativo de gerenciamento de tarefas. O aplicativo permitirá que os usuários criem, visualizem, editem e excluam tarefas. Vou fornecer uma breve explicação do código para facilitar o entendimento:

```swift
import Foundation

// Definindo a estrutura de uma tarefa
struct Task {
    var id: Int
    var title: String
    var description: String
    var priority: String
    
    init(id: Int, title: String, description: String, priority: String) {
        self.id = id
        self.title = title
        self.description = description
        self.priority = priority
    }
}

// Definindo a classe de gerenciamento de tarefas
class TaskManager {
    var tasks: [Task] = []
    
    // Método para adicionar uma nova tarefa
    func addTask(id: Int, title: String, description: String, priority: String) {
        let newTask = Task(id: id, title: title, description: description, priority: priority)
        tasks.append(newTask)
    }
    
    // Método para exibir todas as tarefas
    func displayTasks() {
        for task in tasks {
            print("ID: \(task.id)")
            print("Título: \(task.title)")
            print("Descrição: \(task.description)")
            print("Prioridade: \(task.priority)")
            print("----------------------")
        }
    }
    
    // Método para editar uma tarefa existente
    func editTask(id: Int, title: String, description: String, priority: String) {
        for (index, task) in tasks.enumerated() {
            if task.id == id {
                tasks[index] = Task(id: id, title: title, description: description, priority: priority)
                break
            }
        }
    }
    
    // Método para excluir uma tarefa
    func deleteTask(id: Int) {
        tasks.removeAll { $0.id == id }
    }
}

// Exemplo de uso do aplicativo de gerenciamento de tarefas
let taskManager = TaskManager()

// Adicionando tarefas
taskManager.addTask(id: 1, title: "Comprar mantimentos", description: "Ir ao mercado para comprar mantimentos para a semana", priority: "Alta")
taskManager.addTask(id: 2, title: "Fazer exercícios", description: "Ir à academia para fazer exercícios físicos", priority: "Média")
taskManager.addTask(id: 3, title: "Ler livro", description: "Ler o novo livro de suspense", priority: "Baixa")

// Exibindo todas as tarefas
taskManager.displayTasks()

// Editando uma tarefa existente
taskManager.editTask(id: 2, title: "Fazer exercícios aeróbicos", description: "Ir à academia para fazer exercícios aeróbicos", priority: "Alta")

// Exibindo todas as tarefas novamente
taskManager.displayTasks()

// Excluindo uma tarefa
taskManager.deleteTask(id: 3)

// Exibindo todas as tarefas atualizadas
taskManager.displayTasks()
```

Neste código, começamos definindo a estrutura `Task` que representa uma tarefa com os atributos `id`, `title`, `description` e `priority`. Em seguida, criamos a classe `TaskManager`, responsável por gerenciar as tarefas. Ela possui métodos para adicionar, exibir, editar e excluir tarefas.

No exemplo de uso do aplicativo, criamos uma instância de `TaskManager` chamada `taskManager` e adicionamos algumas tarefas usando o método `addTask`. Em seguida, exibimos todas as tarefas usando o método `displayTasks`. Depois, editamos a segunda tarefa usando o método `editTask`. Em seguida, exibimos todas as tarefas novamente e, por fim, excluímos a terceira tarefa usando o método `deleteTask` e exibimos todas as tarefas atualizadas.