Claro! Aqui está um código complexo em TypeScript que cria uma aplicação de lista de tarefas com funcionalidades avançadas, utilizando React:

```typescript
import React, { useState } from 'react';

interface Tarefa {
  id: number;
  descricao: string;
  concluida: boolean;
}

const App: React.FC = () => {
  const [tarefas, setTarefas] = useState<Tarefa[]>([]);
  const [novaTarefa, setNovaTarefa] = useState('');

  const adicionarTarefa = () => {
    if (novaTarefa.trim() === '') return;

    const novaTarefaObj: Tarefa = {
      id: new Date().getTime(),
      descricao: novaTarefa,
      concluida: false,
    };

    setTarefas([...tarefas, novaTarefaObj]);
    setNovaTarefa('');
  };

  const removerTarefa = (id: number) => {
    const novasTarefas = tarefas.filter((tarefa) => tarefa.id !== id);
    setTarefas(novasTarefas);
  };

  const marcarComoConcluida = (id: number) => {
    const tarefaIndex = tarefas.findIndex((tarefa) => tarefa.id === id);
    if (tarefaIndex === -1) return;

    const tarefaAtualizada = { ...tarefas[tarefaIndex], concluida: true };
    const tarefasAtualizadas = [...tarefas];
    tarefasAtualizadas[tarefaIndex] = tarefaAtualizada;

    setTarefas(tarefasAtualizadas);
  };

  return (
    <div>
      <h1>Lista de Tarefas</h1>

      <div>
        <input
          type="text"
          value={novaTarefa}
          onChange={(e) => setNovaTarefa(e.target.value)}
        />
        <button onClick={adicionarTarefa}>Adicionar</button>
      </div>

      <ul>
        {tarefas.map((tarefa) => (
          <li key={tarefa.id}>
            <span
              style={{
                textDecoration: tarefa.concluida ? 'line-through' : 'none',
              }}
            >
              {tarefa.descricao}
            </span>
            <button onClick={() => removerTarefa(tarefa.id)}>Remover</button>
            {!tarefa.concluida && (
              <button onClick={() => marcarComoConcluida(tarefa.id)}>
                Concluir
              </button>
            )}
          </li>
        ))}
      </ul>
    </div>
  );
};

export default App;
```

Neste código, utilizamos React e TypeScript para criar uma aplicação de lista de tarefas. A aplicação permite adicionar tarefas, remover tarefas e marcar tarefas como concluídas.

A lista de tarefas é armazenada no estado `tarefas`, que é um array de objetos do tipo `Tarefa`. Cada objeto de tarefa possui um `id`, uma `descricao` e um booleano `concluida` para indicar se a tarefa foi concluída ou não.

A função `adicionarTarefa` é chamada quando o usuário clica no botão "Adicionar". Ela verifica se o campo de texto `novaTarefa` não está vazio, cria um novo objeto `Tarefa` com a descrição da nova tarefa e um ID único baseado no timestamp, e adiciona essa nova tarefa ao array `tarefas` utilizando o `setTarefas`.

A função `removerTarefa` é chamada quando o usuário clica no botão "Remover" de uma tarefa. Ela filtra o array `tarefas`, removendo a tarefa com o ID correspondente ao ID passado como parâmetro, e atualiza o estado `tarefas`.

A função `marcarComoConcluida` é chamada quando o usuário clica no botão "Concluir" de uma tarefa. Ela encontra o índice da tarefa no array `tarefas` utilizando o `findIndex`, verifica se encontrou a tarefa correta, atualiza o status `concluida` para `true` e atualiza o estado `tarefas`.

Por fim, no retorno do componente `App`, temos a estrutura da interface do usuário, onde são exibidos o título, o campo de texto para adicionar novas tarefas, a lista de tarefas e os botões para remover e marcar como concluídas as tarefas.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição para ajudar.