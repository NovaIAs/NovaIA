Claro! Vou gerar um código complexo em JavaScript para você. Este código será bastante extenso e diferenciado, com o objetivo de exemplificar um sistema de gerenciamento de tarefas.

```javascript
// Definindo a classe Task para representar uma tarefa
class Task {
  constructor(id, title, description, completed) {
    this.id = id;
    this.title = title;
    this.description = description;
    this.completed = completed;
  }

  // Método para marcar uma tarefa como concluída
  complete() {
    this.completed = true;
  }
}

// Definindo a classe TaskManager para gerenciar as tarefas
class TaskManager {
  constructor() {
    this.tasks = [];
  }

  // Método para adicionar uma nova tarefa
  addTask(title, description) {
    const id = this.tasks.length + 1;
    const completed = false;
    const task = new Task(id, title, description, completed);
    this.tasks.push(task);
  }

  // Método para buscar uma tarefa pelo ID
  getTaskById(id) {
    return this.tasks.find(task => task.id === id);
  }

  // Método para marcar uma tarefa como concluída
  completeTask(id) {
    const task = this.getTaskById(id);
    if (task) {
      task.complete();
    }
  }

  // Método para listar todas as tarefas
  listTasks() {
    this.tasks.forEach(task => {
      console.log(`ID: ${task.id}`);
      console.log(`Título: ${task.title}`);
      console.log(`Descrição: ${task.description}`);
      console.log(`Concluída: ${task.completed}`);
      console.log('---');
    });
  }

  // Método para remover uma tarefa
  removeTask(id) {
    const taskIndex = this.tasks.findIndex(task => task.id === id);
    if (taskIndex !== -1) {
      this.tasks.splice(taskIndex, 1);
    }
  }
}

// Exemplo de uso do sistema de gerenciamento de tarefas
const taskManager = new TaskManager();

taskManager.addTask('Comprar leite', 'Ir ao mercado e comprar leite');
taskManager.addTask('Fazer exercícios', 'Fazer 30 minutos de exercícios físicos');
taskManager.addTask('Estudar JavaScript', 'Revisar os conceitos básicos de JavaScript');

taskManager.completeTask(2);

taskManager.listTasks();

taskManager.removeTask(1);

taskManager.listTasks();
```

Neste código em JavaScript, utilizamos classes para representar as tarefas e o gerenciador de tarefas. A classe `Task` possui propriedades como `id`, `title`, `description` e `completed`, bem como o método `complete()` para marcar uma tarefa como concluída.

A classe `TaskManager` é responsável por adicionar, buscar, completar, listar e remover tarefas. Ela possui um array de tarefas e métodos como `addTask()`, `getTaskById()`, `completeTask()`, `listTasks()` e `removeTask()`.

No exemplo de uso do sistema de gerenciamento de tarefas, criamos uma instância da classe `TaskManager`, adicionamos algumas tarefas, marcamos uma delas como concluída, listamos todas as tarefas e, por fim, removemos uma tarefa e listamos novamente as tarefas atualizadas.

Espero que este código complexo em JavaScript possa te ajudar a entender como implementar um sistema de gerenciamento de tarefas.