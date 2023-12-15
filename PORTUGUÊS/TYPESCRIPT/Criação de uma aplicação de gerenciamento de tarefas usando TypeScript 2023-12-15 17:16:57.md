Claro! Abaixo está um código TypeScript complexo que envolve a criação de uma aplicação de gerenciamento de tarefas usando classes e interfaces:

```typescript
interface Tarefa {
  id: number;
  descricao: string;
  concluida: boolean;
}

class GerenciadorTarefas {
  private tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  adicionarTarefa(descricao: string): void {
    const novaTarefa: Tarefa = {
      id: this.gerarId(),
      descricao,
      concluida: false
    };

    this.tarefas.push(novaTarefa);
    console.log(`Tarefa ${novaTarefa.id} adicionada: ${novaTarefa.descricao}`);
  }

  removerTarefa(id: number): void {
    const index = this.tarefas.findIndex(tarefa => tarefa.id === id);
    if (index !== -1) {
      const tarefaRemovida = this.tarefas.splice(index, 1)[0];
      console.log(`Tarefa ${tarefaRemovida.id} removida: ${tarefaRemovida.descricao}`);
    }
  }

  marcarTarefaConcluida(id: number): void {
    const tarefa = this.getTarefaPorId(id);
    if (tarefa) {
      tarefa.concluida = true;
      console.log(`Tarefa ${tarefa.id} marcada como concluída: ${tarefa.descricao}`);
    }
  }

  listarTarefas(): void {
    console.log('Tarefas:');
    this.tarefas.forEach(tarefa => {
      console.log(`- [${tarefa.concluida ? 'X' : ' '}] ${tarefa.descricao}`);
    });
  }

  private gerarId(): number {
    return Math.floor(Math.random() * 1000);
  }

  private getTarefaPorId(id: number): Tarefa | undefined {
    return this.tarefas.find(tarefa => tarefa.id === id);
  }
}

const gerenciador = new GerenciadorTarefas();
gerenciador.adicionarTarefa('Estudar TypeScript');
gerenciador.adicionarTarefa('Fazer compras');
gerenciador.listarTarefas();
gerenciador.marcarTarefaConcluida(1);
gerenciador.removerTarefa(2);
gerenciador.listarTarefas();
```

Neste código, temos uma interface `Tarefa` que define a estrutura de uma tarefa, com os campos `id`, `descricao` e `concluida`. A classe `GerenciadorTarefas` possui métodos para adicionar, remover e marcar tarefas como concluídas, além de listar todas as tarefas.

No método `adicionarTarefa(descricao: string)`, uma nova tarefa é criada com um ID gerado aleatoriamente e é adicionada ao array de tarefas. No método `removerTarefa(id: number)`, a tarefa com o ID fornecido é encontrada e removida do array. No método `marcarTarefaConcluida(id: number)`, a tarefa com o ID fornecido é encontrada e sua propriedade `concluida` é definida como `true`.

O método `listarTarefas()` exibe todas as tarefas no console, mostrando um marcador de conclusão `[X]` para tarefas concluídas e um espaço em branco para tarefas não concluídas.

No exemplo final, a aplicação cria um objeto `gerenciador` e executa algumas operações, como adicionar tarefas, marcar tarefas como concluídas, remover tarefas e listar todas as tarefas.