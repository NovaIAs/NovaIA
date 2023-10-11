Claro! Aqui está um exemplo de um código complexo em TypeScript que envolve a criação de uma aplicação de gerenciamento de tarefas. Vou dividir o código em partes e explicar cada uma delas:

```typescript
// Importando as dependências necessárias
import express, { Request, Response } from 'express';
import bodyParser from 'body-parser';
import { v4 as uuidv4 } from 'uuid';

// Definindo a estrutura de uma tarefa
interface Task {
  id: string;
  title: string;
  description: string;
  completed: boolean;
}

// Inicializando o servidor express
const app = express();
app.use(bodyParser.json());

// Armazenando as tarefas em memória (simulando um banco de dados)
let tasks: Task[] = [];

// Rota para listar todas as tarefas
app.get('/tasks', (req: Request, res: Response) => {
  res.json(tasks);
});

// Rota para criar uma nova tarefa
app.post('/tasks', (req: Request, res: Response) => {
  const { title, description } = req.body;

  if (!title || !description) {
    return res.status(400).json({ error: 'Faltam informações para criar a tarefa.' });
  }

  const newTask: Task = {
    id: uuidv4(),
    title,
    description,
    completed: false,
  };

  tasks.push(newTask);

  return res.json(newTask);
});

// Rota para atualizar o estado de uma tarefa
app.put('/tasks/:id', (req: Request, res: Response) => {
  const { id } = req.params;
  const { completed } = req.body;

  const task = tasks.find((t) => t.id === id);

  if (!task) {
    return res.status(404).json({ error: 'Tarefa não encontrada.' });
  }

  task.completed = completed;

  return res.json(task);
});

// Rota para deletar uma tarefa
app.delete('/tasks/:id', (req: Request, res: Response) => {
  const { id } = req.params;

  const taskIndex = tasks.findIndex((t) => t.id === id);

  if (taskIndex === -1) {
    return res.status(404).json({ error: 'Tarefa não encontrada.' });
  }

  tasks.splice(taskIndex, 1);

  return res.status(204).send();
});

// Inicializando o servidor na porta 3000
app.listen(3000, () => {
  console.log('Servidor iniciado na porta 3000.');
});
```

Explicação:

1. Importamos as dependências necessárias, incluindo o Express (um framework web para Node.js), Request e Response (do pacote Express) e o Body Parser (para lidar com o corpo das requisições HTTP).

2. Definimos a interface `Task` que representa a estrutura de uma tarefa, com os campos `id`, `title`, `description` e `completed`.

3. Inicializamos o servidor Express e configuramos o Body Parser para lidar com requisições JSON.

4. Criamos uma variável `tasks` para armazenar as tarefas em memória (simulando um banco de dados).

5. Definimos uma rota `GET` para listar todas as tarefas. Quando essa rota é acessada, retornamos as tarefas em formato JSON.

6. Definimos uma rota `POST` para criar uma nova tarefa. Quando essa rota é acessada, extraímos o `title` e `description` do corpo da requisição. Verificamos se essas informações estão presentes e, caso contrário, retornamos um erro. Se estiverem presentes, criamos uma nova tarefa com um ID gerado pelo pacote `uuidv4`, atribuímos os valores de `title`, `description` e `completed` (inicialmente `false`) e adicionamos a nova tarefa ao array `tasks`. Por fim, retornamos a nova tarefa em formato JSON.

7. Definimos uma rota `PUT` para atualizar o estado de uma tarefa. Quando essa rota é acessada, extraímos o `id` do parâmetro e o `completed` do corpo da requisição. Procuramos a tarefa com o ID correspondente no array `tasks`, atualizamos o valor de `completed` e retornamos a tarefa atualizada em formato JSON. Se a tarefa não for encontrada, retornamos um erro.

8. Definimos uma rota `DELETE` para deletar uma tarefa. Quando essa rota é acessada, extraímos o `id` do parâmetro. Procuramos o índice da tarefa com o ID correspondente no array `tasks` e, se encontrarmos, removemos essa tarefa do array. Por fim, retornamos uma resposta vazia com o status `204` (sem conteúdo). Se a tarefa não for encontrada, retornamos um erro.

9. Inicializamos o servidor na porta 3000 e exibimos uma mensagem de log quando o servidor é iniciado.

Espero que este exemplo te ajude a entender como criar um código complexo em TypeScript!