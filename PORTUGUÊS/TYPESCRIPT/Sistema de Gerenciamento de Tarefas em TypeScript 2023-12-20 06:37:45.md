Claro! Vou gerar um código complexo em TypeScript para você. Este código é um exemplo de um sistema de gerenciamento de tarefas. Ele permite criar, atualizar, marcar como concluídas e excluir tarefas.

```typescript
class Tarefa {
  id: number;
  descricao: string;
  concluida: boolean;

  constructor(id: number, descricao: string) {
    this.id = id;
    this.descricao = descricao;
    this.concluida = false;
  }
}

class GerenciadorTarefas {
  tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  adicionarTarefa(descricao: string) {
    const id = this.tarefas.length + 1;
    const tarefa = new Tarefa(id, descricao);
    this.tarefas.push(tarefa);
  }

  atualizarTarefa(id: number, descricao: string) {
    const tarefa = this.tarefas.find(t => t.id === id);
    if (tarefa) {
      tarefa.descricao = descricao;
    }
  }

  marcarTarefaConcluida(id: number) {
    const tarefa = this.tarefas.find(t => t.id === id);
    if (tarefa) {
      tarefa.concluida = true;
    }
  }

  excluirTarefa(id: number) {
    const index = this.tarefas.findIndex(t => t.id === id);
    if (index !== -1) {
      this.tarefas.splice(index, 1);
    }
  }

  listarTarefas() {
    this.tarefas.forEach(tarefa => {
      console.log(`ID: ${tarefa.id}`);
      console.log(`Descrição: ${tarefa.descricao}`);
      console.log(`Concluída: ${tarefa.concluida ? 'Sim' : 'Não'}`);
      console.log('----------------------------------');
    });
  }
}

// Exemplo de uso do sistema

const gerenciador = new GerenciadorTarefas();

gerenciador.adicionarTarefa('Estudar TypeScript');
gerenciador.adicionarTarefa('Fazer compras');

gerenciador.listarTarefas();

gerenciador.atualizarTarefa(2, 'Fazer exercícios físicos');

gerenciador.listarTarefas();

gerenciador.marcarTarefaConcluida(1);

gerenciador.listarTarefas();

gerenciador.excluirTarefa(2);

gerenciador.listarTarefas();
```

Neste código, temos duas classes principais: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa individual, com propriedades como `id`, `descricao` e `concluida`. A classe `GerenciadorTarefas` é responsável por adicionar, atualizar, marcar como concluída, excluir e listar tarefas.

No exemplo de uso do sistema, criamos uma instância do `GerenciadorTarefas` e adicionamos duas tarefas. Em seguida, listamos todas as tarefas, atualizamos a segunda tarefa, marcamos a primeira como concluída, excluímos a segunda e, por fim, listamos novamente as tarefas atualizadas.

Espero que este exemplo atenda às suas expectativas!