Claro! Aqui está um exemplo de um código complexo em TypeScript que envolve a criação de uma aplicação web de gerenciamento de tarefas utilizando o framework Angular:

```typescript
// Importa os módulos necessários do Angular
import { Component, OnInit } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { TaskService } from './task.service';
import { Task } from './task.model';

@Component({
  selector: 'app-task-manager',
  templateUrl: './task-manager.component.html',
  styleUrls: ['./task-manager.component.css']
})
export class TaskManagerComponent implements OnInit {
  tasks: Task[] = [];
  taskForm: FormGroup;
  isEditing: boolean = false;

  constructor(
    private formBuilder: FormBuilder,
    private taskService: TaskService
  ) { }

  ngOnInit(): void {
    this.taskForm = this.formBuilder.group({
      title: ['', Validators.required],
      description: ['', Validators.required]
    });

    this.loadTasks();
  }

  loadTasks(): void {
    this.taskService.getTasks().subscribe((tasks: Task[]) => {
      this.tasks = tasks;
    });
  }

  addTask(): void {
    if (this.taskForm.valid) {
      const newTask: Task = {
        title: this.taskForm.get('title').value,
        description: this.taskForm.get('description').value
      };

      this.taskService.addTask(newTask).subscribe(() => {
        this.loadTasks();
        this.taskForm.reset();
      });
    }
  }

  editTask(task: Task): void {
    this.taskForm.patchValue({
      title: task.title,
      description: task.description
    });

    this.isEditing = true;
  }

  updateTask(): void {
    if (this.taskForm.valid) {
      const updatedTask: Task = {
        title: this.taskForm.get('title').value,
        description: this.taskForm.get('description').value
      };

      this.taskService.updateTask(updatedTask).subscribe(() => {
        this.loadTasks();
        this.taskForm.reset();
        this.isEditing = false;
      });
    }
  }

  deleteTask(task: Task): void {
    if (confirm(`Tem certeza que deseja excluir a tarefa "${task.title}"?`)) {
      this.taskService.deleteTask(task.id).subscribe(() => {
        this.loadTasks();
      });
    }
  }
}
```

Neste exemplo, temos um componente `TaskManagerComponent` que é responsável por exibir, adicionar, editar e excluir tarefas. O componente utiliza um formulário reativo para capturar os detalhes de cada tarefa (título e descrição) e os valida usando o Angular Validators.

O componente se comunica com o serviço `TaskService`, que é responsável por fazer as operações CRUD (Create, Read, Update e Delete) no backend da aplicação. O serviço utiliza o módulo `HttpClient` do Angular para fazer as requisições HTTP para a API.

Além disso, temos uma interface `Task` que define a estrutura de uma tarefa, com propriedades `title` e `description`.

O código também inclui a importação dos módulos e serviços necessários, a definição do template HTML e estilização CSS correspondentes ao componente.

Espero que este exemplo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, por favor, me avise.