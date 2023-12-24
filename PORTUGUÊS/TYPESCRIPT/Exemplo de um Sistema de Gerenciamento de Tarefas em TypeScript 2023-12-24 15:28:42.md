Claro! Aqui está um exemplo de um código complexo em TypeScript que utiliza classes e interfaces para criar um sistema de gerenciamento de tarefas:

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
    const id = this.tarefas.length + 1;
    const novaTarefa: Tarefa = {
      id,
      descricao,
      concluida: false,
    };
    this.tarefas.push(novaTarefa);
  }

  listarTarefas(): void {
    console.log("Lista de Tarefas:");
    if (this.tarefas.length === 0) {
      console.log("Nenhuma tarefa encontrada.");
    } else {
      this.tarefas.forEach((tarefa) => {
        console.log(
          `ID: ${tarefa.id} - Descrição: ${tarefa.descricao} - Concluída: ${
            tarefa.concluida ? "Sim" : "Não"
          }`
        );
      });
    }
  }

  marcarTarefaConcluida(id: number): void {
    const tarefa = this.tarefas.find((t) => t.id === id);
    if (tarefa) {
      tarefa.concluida = true;
      console.log(`Tarefa ${id} marcada como concluída.`);
    } else {
      console.log(`Tarefa ${id} não encontrada.`);
    }
  }

  removerTarefa(id: number): void {
    const index = this.tarefas.findIndex((t) => t.id === id);
    if (index !== -1) {
      this.tarefas.splice(index, 1);
      console.log(`Tarefa ${id} removida com sucesso.`);
    } else {
      console.log(`Tarefa ${id} não encontrada.`);
    }
  }
}

// Exemplo de uso do GerenciadorTarefas
const gerenciador = new GerenciadorTarefas();

gerenciador.adicionarTarefa("Estudar TypeScript");
gerenciador.adicionarTarefa("Fazer compras");
gerenciador.adicionarTarefa("Ler livro");

gerenciador.listarTarefas();

gerenciador.marcarTarefaConcluida(2);
gerenciador.removerTarefa(3);

gerenciador.listarTarefas();
```

Neste código, temos a definição da interface `Tarefa` que representa uma tarefa com os campos `id`, `descricao` e `concluida`. Em seguida, temos a classe `GerenciadorTarefas` que possui métodos para adicionar, listar, marcar como concluída e remover tarefas.

No exemplo de uso do `GerenciadorTarefas`, criamos uma instância do gerenciador, adicionamos algumas tarefas, listamos todas as tarefas, marcamos a segunda como concluída e removemos a terceira. Por fim, listamos novamente as tarefas atualizadas.

Espero que este código atenda às suas expectativas!