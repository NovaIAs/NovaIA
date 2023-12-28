Claro! Aqui está um exemplo de código em TypeScript que envolve a criação de uma aplicação de gerenciamento de tarefas. Essa aplicação permite adicionar, editar, excluir e marcar tarefas como concluídas. Vamos dividir o código em diferentes módulos para facilitar o entendimento.

**Módulo de Tarefa**

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

  public isConcluida(): boolean {
    return this.concluida;
  }

  public marcarComoConcluida(): void {
    this.concluida = true;
  }
}
```

Neste módulo, definimos a classe `Tarefa` que representa uma única tarefa. Ela possui uma propriedade privada `descricao` que armazena a descrição da tarefa e uma propriedade privada `concluida` que indica se a tarefa foi concluída ou não. A classe possui métodos para obter a descrição da tarefa, verificar se está concluída e marcar a tarefa como concluída.

**Módulo de Lista de Tarefas**

```typescript
class ListaTarefas {
  private tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  public adicionarTarefa(descricao: string): void {
    const tarefa = new Tarefa(descricao);
    this.tarefas.push(tarefa);
  }

  public editarTarefa(index: number, novaDescricao: string): void {
    if (index >= 0 && index < this.tarefas.length) {
      const tarefa = this.tarefas[index];
      tarefa.descricao = novaDescricao;
    }
  }

  public excluirTarefa(index: number): void {
    if (index >= 0 && index < this.tarefas.length) {
      this.tarefas.splice(index, 1);
    }
  }

  public marcarTarefaComoConcluida(index: number): void {
    if (index >= 0 && index < this.tarefas.length) {
      const tarefa = this.tarefas[index];
      tarefa.marcarComoConcluida();
    }
  }

  public listarTarefas(): void {
    this.tarefas.forEach((tarefa, index) => {
      console.log(`${index + 1}. ${tarefa.getDescricao()} - ${tarefa.isConcluida() ? 'Concluída' : 'Pendente'}`);
    });
  }
}
```

Neste módulo, definimos a classe `ListaTarefas` que representa uma lista de tarefas. Ela possui uma propriedade privada `tarefas` que armazena as tarefas adicionadas à lista. A classe possui métodos para adicionar uma nova tarefa, editar uma tarefa existente, excluir uma tarefa, marcar uma tarefa como concluída e listar todas as tarefas.

**Módulo Principal**

```typescript
const listaTarefas = new ListaTarefas();

listaTarefas.adicionarTarefa('Fazer compras');
listaTarefas.adicionarTarefa('Estudar programação');
listaTarefas.adicionarTarefa('Ler um livro');

listaTarefas.listarTarefas();

listaTarefas.marcarTarefaComoConcluida(1);

listaTarefas.listarTarefas();

listaTarefas.editarTarefa(2, 'Assistir um filme');

listaTarefas.listarTarefas();

listaTarefas.excluirTarefa(0);

listaTarefas.listarTarefas();
```

Neste módulo, criamos uma instância da classe `ListaTarefas` e realizamos algumas operações, como adicionar tarefas, listar tarefas, marcar uma tarefa como concluída, editar uma tarefa e excluir uma tarefa. Ao executar esse código, você verá a saída no console mostrando o estado das tarefas em cada etapa.

Esse exemplo de código mostra como criar uma aplicação básica de gerenciamento de tarefas em TypeScript. Você pode expandir esse código adicionando recursos adicionais, como persistência de dados, autenticação de usuário, etc.