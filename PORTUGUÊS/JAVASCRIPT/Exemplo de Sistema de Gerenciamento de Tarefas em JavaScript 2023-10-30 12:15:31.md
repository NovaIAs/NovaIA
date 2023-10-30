Claro! Aqui está um exemplo de um código complexo em JavaScript que realiza uma simulação de um sistema de gerenciamento de tarefas:

```javascript
// Definição da classe Task
class Task {
  constructor(title, description, priority) {
    this.title = title;
    this.description = description;
    this.priority = priority;
    this.completed = false;
  }

  complete() {
    this.completed = true;
  }

  toString() {
    return `${this.title} - ${this.description} - ${this.priority} - ${this.completed ? 'Concluída' : 'Pendente'}`;
  }
}

// Definição da classe TaskManager
class TaskManager {
  constructor() {
    this.tasks = [];
  }

  addTask(title, description, priority) {
    const task = new Task(title, description, priority);
    this.tasks.push(task);
  }

  completeTask(taskIndex) {
    if (taskIndex >= 0 && taskIndex < this.tasks.length) {
      this.tasks[taskIndex].complete();
    }
  }

  getTasks() {
    return this.tasks;
  }

  toString() {
    let result = 'Lista de Tarefas:\n';
    this.tasks.forEach((task, index) => {
      result += `${index + 1}. ${task.toString()}\n`;
    });
    return result;
  }
}

// Utilização do sistema de gerenciamento de tarefas
const taskManager = new TaskManager();
taskManager.addTask('Estudar JavaScript', 'Ler o livro de JavaScript', 'Alta');
taskManager.addTask('Fazer exercícios', 'Resolver os exercícios práticos de JavaScript', 'Média');
taskManager.addTask('Preparar apresentação', 'Criar slides para apresentar o projeto', 'Baixa');

taskManager.completeTask(1);

console.log(taskManager.toString());
```

Nesse código, temos duas classes principais: `Task` e `TaskManager`. A classe `Task` representa uma tarefa, contendo propriedades como título, descrição, prioridade e uma flag para indicar se a tarefa foi concluída. Ela possui métodos para marcar a tarefa como concluída e para retornar uma representação em formato de string.

Já a classe `TaskManager` é responsável por gerenciar um conjunto de tarefas. Ela possui métodos para adicionar tarefas, marcar tarefas como concluídas e retornar a lista de tarefas em formato de string.

No exemplo de uso, criamos uma instância de `TaskManager`, adicionamos três tarefas diferentes e marcamos a segunda tarefa como concluída. Por fim, imprimimos a lista de tarefas no console.

Esse código é apenas um exemplo de um sistema de gerenciamento de tarefas em JavaScript, e pode ser adaptado e expandido de acordo com as necessidades do projeto.