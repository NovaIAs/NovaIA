Claro! Aqui está um código em TypeScript que implementa um sistema de gerenciamento de tarefas usando classes e interfaces:

```typescript
interface Tarefa {
  id: number;
  descricao: string;
  concluida: boolean;
}

class GerenciadorTarefas {
  private tarefas: Tarefa[] = [];

  adicionarTarefa(tarefa: Tarefa): void {
    this.tarefas.push(tarefa);
  }

  listarTarefas(): void {
    console.log("Listando todas as tarefas:");

    this.tarefas.forEach((tarefa) => {
      console.log(`ID: ${tarefa.id}`);
      console.log(`Descrição: ${tarefa.descricao}`);
      console.log(`Concluída: ${tarefa.concluida}`);
      console.log("-----------------------------");
    });
  }

  buscarTarefaPorId(id: number): Tarefa | undefined {
    return this.tarefas.find((tarefa) => tarefa.id === id);
  }

  marcarTarefaComoConcluida(id: number): void {
    const tarefa = this.buscarTarefaPorId(id);

    if (tarefa) {
      tarefa.concluida = true;
      console.log(`Tarefa ${id} marcada como concluída.`);
    } else {
      console.log(`Tarefa ${id} não encontrada.`);
    }
  }

  removerTarefa(id: number): void {
    const indice = this.tarefas.findIndex((tarefa) => tarefa.id === id);

    if (indice !== -1) {
      this.tarefas.splice(indice, 1);
      console.log(`Tarefa ${id} removida.`);
    } else {
      console.log(`Tarefa ${id} não encontrada.`);
    }
  }
}

// Exemplo de uso do gerenciador de tarefas

const gerenciador = new GerenciadorTarefas();

const tarefa1: Tarefa = {
  id: 1,
  descricao: "Estudar TypeScript",
  concluida: false,
};

const tarefa2: Tarefa = {
  id: 2,
  descricao: "Fazer exercícios de JavaScript",
  concluida: false,
};

gerenciador.adicionarTarefa(tarefa1);
gerenciador.adicionarTarefa(tarefa2);

gerenciador.listarTarefas();

gerenciador.marcarTarefaComoConcluida(1);

gerenciador.removerTarefa(2);

gerenciador.listarTarefas();
```

Neste código, temos uma interface `Tarefa` que define a estrutura de uma tarefa, com propriedades `id`, `descricao` e `concluida`. Em seguida, temos uma classe `GerenciadorTarefas` que implementa métodos para adicionar tarefas, listar tarefas, buscar tarefa por ID, marcar uma tarefa como concluída e remover uma tarefa.

No exemplo de uso, criamos duas tarefas, as adicionamos ao gerenciador de tarefas, listamos as tarefas, marcamos a primeira tarefa como concluída, removemos a segunda tarefa e listamos as tarefas atualizadas.