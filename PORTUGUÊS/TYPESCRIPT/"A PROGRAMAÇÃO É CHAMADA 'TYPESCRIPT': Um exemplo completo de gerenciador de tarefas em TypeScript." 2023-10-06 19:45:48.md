Claro! Aqui está um código complexo em TypeScript que cria uma aplicação de gerenciamento de tarefas:

```typescript
// Definição da classe Task para representar uma tarefa
class Task {
  private title: string;
  private description: string;
  private priority: number;
  private completed: boolean;

  constructor(title: string, description: string, priority: number) {
    this.title = title;
    this.description = description;
    this.priority = priority;
    this.completed = false;
  }

  getTitle(): string {
    return this.title;
  }

  getDescription(): string {
    return this.description;
  }

  getPriority(): number {
    return this.priority;
  }

  isCompleted(): boolean {
    return this.completed;
  }

  markAsCompleted(): void {
    this.completed = true;
  }
}

// Definição da classe TaskManager para gerenciar as tarefas
class TaskManager {
  private tasks: Task[];

  constructor() {
    this.tasks = [];
  }

  addTask(title: string, description: string, priority: number): void {
    const task = new Task(title, description, priority);
    this.tasks.push(task);
  }

  getTasks(): Task[] {
    return this.tasks;
  }

  getCompletedTasks(): Task[] {
    return this.tasks.filter(task => task.isCompleted());
  }

  getTasksByPriority(priority: number): Task[] {
    return this.tasks.filter(task => task.getPriority() === priority);
  }

  markTaskAsCompleted(title: string): void {
    const task = this.tasks.find(task => task.getTitle() === title);
    if (task) {
      task.markAsCompleted();
    }
  }
}

// Utilização do TaskManager para criar e gerenciar tarefas
const taskManager = new TaskManager();

taskManager.addTask("Comprar mantimentos", "Ir ao mercado e comprar mantimentos para a semana.", 2);
taskManager.addTask("Lavar o carro", "Levar o carro para lavar e aspirar o interior.", 1);
taskManager.addTask("Fazer exercícios", "Fazer 30 minutos de exercícios físicos.", 3);

console.log("Todas as tarefas:");
console.log(taskManager.getTasks());

console.log("Tarefas completadas:");
console.log(taskManager.getCompletedTasks());

console.log("Tarefas de prioridade 2:");
console.log(taskManager.getTasksByPriority(2));

taskManager.markTaskAsCompleted("Comprar mantimentos");

console.log("Todas as tarefas após marcar 'Comprar mantimentos' como concluída:");
console.log(taskManager.getTasks());
```

Neste código, criamos duas classes principais: `Task` e `TaskManager`. A classe `Task` representa uma tarefa individual, com propriedades como título, descrição, prioridade e status de conclusão. A classe `TaskManager` é responsável por gerenciar a lista de tarefas, incluindo adicionar tarefas, obter tarefas, marcar tarefas como concluídas, entre outras funcionalidades.

Na parte final do código, criamos uma instância do `TaskManager` e adicionamos algumas tarefas. Em seguida, imprimimos as tarefas na lista, as tarefas concluídas, as tarefas com prioridade 2 e marcamos uma tarefa como concluída. Por fim, imprimimos novamente a lista de tarefas para verificar as alterações.