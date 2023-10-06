Claro! Aqui está um código complexo em TypeScript que demonstra um sistema de gerenciamento de tarefas. O código é dividido em várias classes, cada uma responsável por uma funcionalidade específica. Vou explicar cada parte em detalhes:

```typescript
class Tarefa {
  private descricao: string;
  private concluida: boolean;

  constructor(descricao: string) {
    this.descricao = descricao;
    this.concluida = false;
  }

  public getDescricao(): string {
    return this.descricao;
  }

  public marcarConcluida(): void {
    this.concluida = true;
  }

  public estaConcluida(): boolean {
    return this.concluida;
  }
}

class ListaTarefas {
  private tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  public adicionarTarefa(descricao: string): void {
    const tarefa = new Tarefa(descricao);
    this.tarefas.push(tarefa);
  }

  public listarTarefas(): void {
    this.tarefas.forEach((tarefa, index) => {
      console.log(`${index + 1}. ${tarefa.getDescricao()} - ${tarefa.estaConcluida() ? 'Concluída' : 'Pendente'}`);
    });
  }

  public marcarTarefaConcluida(indice: number): void {
    const tarefa = this.tarefas[indice - 1];
    if (tarefa) {
      tarefa.marcarConcluida();
    }
  }
}

class Aplicacao {
  private listaTarefas: ListaTarefas;

  constructor() {
    this.listaTarefas = new ListaTarefas();
  }

  public adicionarTarefa(descricao: string): void {
    this.listaTarefas.adicionarTarefa(descricao);
  }

  public listarTarefas(): void {
    this.listaTarefas.listarTarefas();
  }

  public marcarTarefaConcluida(indice: number): void {
    this.listaTarefas.marcarTarefaConcluida(indice);
  }
}

// Exemplo de uso da aplicação

const aplicacao = new Aplicacao();

aplicacao.adicionarTarefa('Fazer compras');
aplicacao.adicionarTarefa('Estudar TypeScript');
aplicacao.adicionarTarefa('Fazer exercícios físicos');

aplicacao.listarTarefas(); // Lista todas as tarefas adicionadas

aplicacao.marcarTarefaConcluida(2); // Marca a segunda tarefa como concluída

aplicacao.listarTarefas(); // Lista novamente as tarefas, mostrando a segunda como concluída
```

Neste código, temos três classes principais: `Tarefa`, `ListaTarefas` e `Aplicacao`.

A classe `Tarefa` representa uma única tarefa, com uma descrição e um estado de conclusão. Ela possui métodos para obter a descrição, marcar como concluída e verificar se está concluída.

A classe `ListaTarefas` é responsável por gerenciar uma lista de tarefas. Ela possui um array de objetos `Tarefa` e métodos para adicionar, listar e marcar tarefas como concluídas.

A classe `Aplicacao` é a camada de interface do sistema. Ela possui uma instância da classe `ListaTarefas` e métodos para interagir com ela, como adicionar tarefas, listar tarefas e marcar tarefas como concluídas.

No exemplo de uso da aplicação, criamos uma instância da classe `Aplicacao` e adicionamos três tarefas. Em seguida, listamos as tarefas e marcamos a segunda como concluída. Por fim, listamos novamente as tarefas para verificar a atualização do estado de conclusão.

Esse código é apenas uma demonstração simples de um sistema de gerenciamento de tarefas em TypeScript. Você pode personalizá-lo e adicionar mais funcionalidades conforme suas necessidades.