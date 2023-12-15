Claro! Aqui está um exemplo de código em TypeScript que cria uma aplicação de lista de tarefas com funcionalidades avançadas:

```typescript
class Tarefa {
  constructor(public descricao: string, public concluida: boolean) {}
}

class ListaTarefas {
  private tarefas: Tarefa[] = [];

  adicionarTarefa(descricao: string): void {
    const novaTarefa = new Tarefa(descricao, false);
    this.tarefas.push(novaTarefa);
  }

  removerTarefa(index: number): void {
    this.tarefas.splice(index, 1);
  }

  marcarTarefaComoConcluida(index: number): void {
    this.tarefas[index].concluida = true;
  }

  listarTarefas(): void {
    console.log("Lista de Tarefas:");
    this.tarefas.forEach((tarefa, index) => {
      const status = tarefa.concluida ? "Concluída" : "Pendente";
      console.log(`${index + 1}. ${tarefa.descricao} (${status})`);
    });
  }
}

class Aplicacao {
  private listaTarefas: ListaTarefas = new ListaTarefas();

  adicionarTarefa(descricao: string): void {
    this.listaTarefas.adicionarTarefa(descricao);
    console.log(`Tarefa "${descricao}" adicionada com sucesso!`);
  }

  removerTarefa(index: number): void {
    this.listaTarefas.removerTarefa(index);
    console.log(`Tarefa removida com sucesso!`);
  }

  concluirTarefa(index: number): void {
    this.listaTarefas.marcarTarefaComoConcluida(index);
    console.log(`Tarefa concluída com sucesso!`);
  }

  listarTarefas(): void {
    this.listaTarefas.listarTarefas();
  }
}

// Uso da aplicação

const aplicacao = new Aplicacao();

aplicacao.adicionarTarefa("Estudar TypeScript");
aplicacao.adicionarTarefa("Fazer compras");
aplicacao.listarTarefas();

aplicacao.concluirTarefa(0);
aplicacao.listarTarefas();

aplicacao.removerTarefa(1);
aplicacao.listarTarefas();
```

Neste código, temos três classes: `Tarefa`, `ListaTarefas` e `Aplicacao`. 

- A classe `Tarefa` representa uma tarefa individual, contendo uma descrição e um status de conclusão.
- A classe `ListaTarefas` é responsável por gerenciar uma lista de tarefas. Ela possui métodos para adicionar, remover e marcar tarefas como concluídas, bem como listar todas as tarefas.
- A classe `Aplicacao` é a camada de interface da aplicação. Ela encapsula o uso da classe `ListaTarefas` e fornece métodos para interagir com a lista de tarefas, como adicionar, remover, concluir e listar tarefas.

No exemplo de uso da aplicação, criamos uma instância da classe `Aplicacao` e utilizamos seus métodos para adicionar duas tarefas, listar as tarefas, concluir a primeira tarefa, remover a segunda tarefa e listar novamente as tarefas.

Espero que este exemplo atenda às suas expectativas!