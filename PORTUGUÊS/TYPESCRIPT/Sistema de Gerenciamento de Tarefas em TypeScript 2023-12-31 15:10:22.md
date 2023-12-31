Claro! Aqui está um exemplo de código complexo em TypeScript que envolve a criação de um sistema de gerenciamento de tarefas:

```typescript
class Tarefa {
  private titulo: string;
  private descricao: string;
  private dataLimite: Date;
  private concluida: boolean;

  constructor(titulo: string, descricao: string, dataLimite: Date) {
    this.titulo = titulo;
    this.descricao = descricao;
    this.dataLimite = dataLimite;
    this.concluida = false;
  }

  public marcarComoConcluida(): void {
    this.concluida = true;
  }

  public getTitulo(): string {
    return this.titulo;
  }

  public getDescricao(): string {
    return this.descricao;
  }

  public getDataLimite(): Date {
    return this.dataLimite;
  }

  public isConcluida(): boolean {
    return this.concluida;
  }
}

class GerenciadorTarefas {
  private tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  public adicionarTarefa(tarefa: Tarefa): void {
    this.tarefas.push(tarefa);
  }

  public listarTarefas(): void {
    if (this.tarefas.length === 0) {
      console.log("Não há tarefas cadastradas.");
    } else {
      console.log("Tarefas cadastradas:");
      this.tarefas.forEach((tarefa, index) => {
        console.log(`[${index + 1}] Título: ${tarefa.getTitulo()}`);
        console.log(`    Descrição: ${tarefa.getDescricao()}`);
        console.log(`    Data Limite: ${tarefa.getDataLimite().toLocaleDateString()}`);
        console.log(`    Concluída: ${tarefa.isConcluida() ? 'Sim' : 'Não'}`);
        console.log();
      });
    }
  }

  public marcarTarefaComoConcluida(indice: number): void {
    if (indice < 1 || indice > this.tarefas.length) {
      console.log("Índice inválido.");
    } else {
      const tarefa = this.tarefas[indice - 1];
      tarefa.marcarComoConcluida();
      console.log(`A tarefa "${tarefa.getTitulo()}" foi marcada como concluída.`);
    }
  }
}

// Exemplo de uso
const gerenciador = new GerenciadorTarefas();

const tarefa1 = new Tarefa("Estudar", "Estudar para a prova de matemática", new Date(2022, 10, 15));
const tarefa2 = new Tarefa("Ler", "Ler o livro recomendado pelo professor", new Date(2022, 10, 20));
const tarefa3 = new Tarefa("Fazer exercícios", "Resolver os exercícios do livro de física", new Date(2022, 10, 30));

gerenciador.adicionarTarefa(tarefa1);
gerenciador.adicionarTarefa(tarefa2);
gerenciador.adicionarTarefa(tarefa3);

gerenciador.listarTarefas();
gerenciador.marcarTarefaComoConcluida(2);

gerenciador.listarTarefas();
```

Neste código, temos duas classes principais: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa individual com propriedades como título, descrição, data limite e status de conclusão. Ela também possui métodos para marcar a tarefa como concluída e obter informações sobre a tarefa.

A classe `GerenciadorTarefas` é responsável por armazenar e gerenciar todas as tarefas. Ela possui métodos para adicionar tarefas, listar todas as tarefas cadastradas e marcar uma tarefa como concluída com base em seu índice.

No exemplo de uso no final do código, criamos algumas tarefas, as adicionamos ao gerenciador e executamos algumas operações, como listar as tarefas e marcar uma delas como concluída.

Espero que este código atenda às suas expectativas! Lembre-se de que esse é apenas um exemplo e você pode personalizá-lo de acordo com suas necessidades.