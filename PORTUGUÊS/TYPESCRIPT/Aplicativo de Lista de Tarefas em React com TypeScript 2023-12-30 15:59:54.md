Claro! Aqui está um código em TypeScript que cria uma aplicação de lista de tarefas usando o React. A aplicação permite adicionar, remover e marcar tarefas como concluídas.

```typescript
import React, { useState } from 'react';

type Task = {
  id: number;
  description: string;
  completed: boolean;
};

const TodoApp: React.FC = () => {
  const [tasks, setTasks] = useState<Task[]>([]);
  const [newTask, setNewTask] = useState('');

  const addTask = () => {
    if (newTask.trim() !== '') {
      const task: Task = {
        id: new Date().getTime(),
        description: newTask,
        completed: false,
      };
      setTasks([...tasks, task]);
      setNewTask('');
    }
  };

  const removeTask = (id: number) => {
    const updatedTasks = tasks.filter((task) => task.id !== id);
    setTasks(updatedTasks);
  };

  const toggleComplete = (id: number) => {
    const updatedTasks = tasks.map((task) => {
      if (task.id === id) {
        return { ...task, completed: !task.completed };
      }
      return task;
    });
    setTasks(updatedTasks);
  };

  return (
    <div>
      <h1>Lista de Tarefas</h1>
      <input
        type="text"
        value={newTask}
        onChange={(e) => setNewTask(e.target.value)}
      />
      <button onClick={addTask}>Adicionar Tarefa</button>
      <ul>
        {tasks.map((task) => (
          <li key={task.id}>
            <span
              style={{
                textDecoration: task.completed ? 'line-through' : 'none',
              }}
            >
              {task.description}
            </span>
            <button onClick={() => toggleComplete(task.id)}>
              {task.completed ? 'Desmarcar' : 'Marcar'} como Concluída
            </button>
            <button onClick={() => removeTask(task.id)}>Remover</button>
          </li>
        ))}
      </ul>
    </div>
  );
};

export default TodoApp;
```

Este código cria um componente chamado `TodoApp` que representa a aplicação de lista de tarefas. A aplicação mantém um estado de `tasks`, que é uma matriz de objetos `Task`, e um estado de `newTask`, que é uma string representando a nova tarefa a ser adicionada.

O método `addTask` é chamado quando o botão "Adicionar Tarefa" é clicado. Ele cria um novo objeto `Task` com um ID único gerado a partir da data e hora atual, a descrição da tarefa e define a conclusão como `false`. Em seguida, adiciona a nova tarefa à matriz `tasks` e redefine o estado de `newTask` como uma string vazia.

Os métodos `removeTask` e `toggleComplete` são responsáveis por remover uma tarefa existente e alternar o status de conclusão de uma tarefa, respectivamente.

No retorno JSX, uma lista de tarefas é renderizada usando `tasks.map()`. Cada tarefa é exibida como um item de lista com a descrição da tarefa, um botão para marcar ou desmarcar como concluída e um botão para remover a tarefa.

Esse código em TypeScript utiliza a biblioteca React para criar uma interface de usuário interativa e tipagem estática para garantir maior segurança durante o desenvolvimento.