Certamente! Aqui está um código complexo em JavaScript que envolve a criação de uma aplicação de lista de tarefas com recursos avançados:

```javascript
// Definindo a classe Task para representar uma tarefa
class Task {
  constructor(description, priority, dueDate) {
    this.description = description;
    this.priority = priority;
    this.dueDate = dueDate;
    this.isCompleted = false;
  }

  completeTask() {
    this.isCompleted = true;
  }

  toString() {
    return `${this.description} (Prioridade: ${this.priority}, Data de Vencimento: ${this.dueDate}, Completa: ${this.isCompleted})`;
  }
}

// Definindo a classe TaskList para gerenciar a lista de tarefas
class TaskList {
  constructor() {
    this.tasks = [];
  }

  addTask(task) {
    this.tasks.push(task);
  }

  removeTask(index) {
    if (index >= 0 && index < this.tasks.length) {
      this.tasks.splice(index, 1);
    }
  }

  completeTask(index) {
    if (index >= 0 && index < this.tasks.length) {
      this.tasks[index].completeTask();
    }
  }

  getTasks() {
    return this.tasks;
  }
}

// Criando uma instância da classe TaskList
const taskList = new TaskList();

// Adicionando algumas tarefas à lista
const task1 = new Task("Estudar JavaScript", "Alta", "2022-01-30");
const task2 = new Task("Fazer compras", "Média", "2022-02-05");
const task3 = new Task("Limpar a casa", "Baixa", "2022-02-10");

taskList.addTask(task1);
taskList.addTask(task2);
taskList.addTask(task3);

// Completando a segunda tarefa
taskList.completeTask(1);

// Removendo a primeira tarefa
taskList.removeTask(0);

// Exibindo todas as tarefas na lista
const tasks = taskList.getTasks();

tasks.forEach((task, index) => {
  console.log(`Tarefa ${index + 1}: ${task.toString()}`);
});
```

Neste código, criamos duas classes: `Task` (representando uma tarefa) e `TaskList` (gerenciando a lista de tarefas). A classe `Task` possui propriedades como descrição, prioridade, data de vencimento e status de conclusão. Além disso, possui métodos para marcar a tarefa como concluída e converter a tarefa em uma representação de string.

A classe `TaskList` é responsável por adicionar, remover e completar tarefas na lista. Ela mantém um array de tarefas e fornece métodos para manipulá-las. 

No exemplo fornecido, criamos uma instância da classe `TaskList`, adicionamos algumas tarefas, marcamos a segunda tarefa como concluída e removemos a primeira tarefa. Por fim, exibimos todas as tarefas restantes na lista.

Espero que este exemplo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.