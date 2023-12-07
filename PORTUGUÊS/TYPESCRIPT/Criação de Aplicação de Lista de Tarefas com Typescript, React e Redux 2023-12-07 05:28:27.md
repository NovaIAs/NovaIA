Claro! Aqui está um código complexo em Typescript que cria uma aplicação de lista de tarefas utilizando React e Redux:

```typescript
// Importações
import React, { useState } from "react";
import { createStore } from "redux";
import { Provider, useDispatch, useSelector } from "react-redux";

// Definindo os tipos
type RootState = {
  tasks: Task[];
};

type Task = {
  id: number;
  text: string;
  completed: boolean;
};

type AddTaskAction = {
  type: "ADD_TASK";
  payload: Task;
};

type ToggleTaskAction = {
  type: "TOGGLE_TASK";
  payload: number;
};

type TaskAction = AddTaskAction | ToggleTaskAction;

// Ações
const addTask = (task: Task): AddTaskAction => ({
  type: "ADD_TASK",
  payload: task,
});

const toggleTask = (id: number): ToggleTaskAction => ({
  type: "TOGGLE_TASK",
  payload: id,
});

// Redutor
const tasksReducer = (state: Task[] = [], action: TaskAction): Task[] => {
  switch (action.type) {
    case "ADD_TASK":
      return [...state, action.payload];
    case "TOGGLE_TASK":
      return state.map((task) =>
        task.id === action.payload ? { ...task, completed: !task.completed } : task
      );
    default:
      return state;
  }
};

// Criação da store
const store = createStore(tasksReducer);

// Componente Lista de Tarefas
const TaskList: React.FC = () => {
  const tasks = useSelector((state: RootState) => state.tasks);
  const dispatch = useDispatch();
  const [input, setInput] = useState<string>("");

  const handleAddTask = () => {
    const newTask: Task = {
      id: Math.random(),
      text: input,
      completed: false,
    };
    dispatch(addTask(newTask));
    setInput("");
  };

  const handleToggleTask = (id: number) => {
    dispatch(toggleTask(id));
  };

  return (
    <div>
      <h1>Lista de Tarefas</h1>
      <input
        type="text"
        value={input}
        onChange={(e) => setInput(e.target.value)}
      />
      <button onClick={handleAddTask}>Adicionar</button>
      <ul>
        {tasks.map((task) => (
          <li
            key={task.id}
            onClick={() => handleToggleTask(task.id)}
            style={{
              textDecoration: task.completed ? "line-through" : "none",
              cursor: "pointer",
            }}
          >
            {task.text}
          </li>
        ))}
      </ul>
    </div>
  );
};

// Componente principal
const App: React.FC = () => (
  <Provider store={store}>
    <TaskList />
  </Provider>
);

export default App;
```

Neste código, utilizei Typescript juntamente com React e Redux para criar uma aplicação de Lista de Tarefas. No início, são definidos os tipos para o estado da aplicação, as ações disponíveis e as tarefas em si.

Em seguida, são definidas as ações de adicionar uma nova tarefa e alternar o estado de uma tarefa (completada ou não). 

Depois, é criado o redutor tasksReducer que trata essas ações e atualiza o estado das tarefas de acordo com cada ação.

Após isso, é criada a store utilizando createStore do Redux.

Então, é criado o componente TaskList que utiliza Hooks (useState, useDispatch, useSelector) para gerenciar o estado e as ações através da store.

Por fim, o componente App é criado, envolvendo o componente TaskList com o Provider do Redux para fornecer a store para a aplicação.

Este código é um exemplo de como utilizar o React com Redux e Typescript para criar uma aplicação mais complexa com gerenciamento de estados.