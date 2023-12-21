Claro! Aqui está um exemplo de código complexo em TypeScript que cria uma aplicação de lista de tarefas:

```typescript
// Definição da interface para uma tarefa
interface Tarefa {
  id: number;
  descricao: string;
  concluida: boolean;
}

// Classe para manipulação da lista de tarefas
class ListaTarefas {
  private tarefas: Tarefa[];
  private proximoId: number;

  constructor() {
    this.tarefas = [];
    this.proximoId = 1;
  }

  // Adiciona uma nova tarefa à lista
  adicionarTarefa(descricao: string): void {
    const novaTarefa: Tarefa = {
      id: this.proximoId,
      descricao,
      concluida: false,
    };
    this.tarefas.push(novaTarefa);
    this.proximoId++;
  }

  // Marca uma tarefa como concluída
  marcarConcluida(id: number): void {
    const tarefa = this.buscarTarefaPorId(id);
    if (tarefa) {
      tarefa.concluida = true;
    }
  }

  // Remove uma tarefa da lista
  removerTarefa(id: number): void {
    const index = this.tarefas.findIndex((tarefa) => tarefa.id === id);
    if (index !== -1) {
      this.tarefas.splice(index, 1);
    }
  }

  // Retorna uma tarefa com base no ID
  buscarTarefaPorId(id: number): Tarefa | undefined {
    return this.tarefas.find((tarefa) => tarefa.id === id);
  }

  // Lista todas as tarefas
  listarTarefas(): void {
    console.log("Lista de Tarefas:");
    this.tarefas.forEach((tarefa) => {
      const status = tarefa.concluida ? "[x]" : "[ ]";
      console.log(`${status} ${tarefa.id}: ${tarefa.descricao}`);
    });
  }
}

// Uso da classe ListaTarefas
const listaTarefas = new ListaTarefas();
listaTarefas.adicionarTarefa("Estudar TypeScript");
listaTarefas.adicionarTarefa("Fazer compras");
listaTarefas.listarTarefas();
listaTarefas.marcarConcluida(1);
listaTarefas.removerTarefa(2);
listaTarefas.listarTarefas();
```

Neste exemplo, temos uma classe `ListaTarefas` que representa a lista de tarefas. A classe possui métodos para adicionar uma nova tarefa, marcar uma tarefa como concluída, remover uma tarefa e listar todas as tarefas.

Cada tarefa é representada por uma interface `Tarefa`, que possui os campos `id` (identificador único), `descricao` (descrição da tarefa) e `concluida` (indica se a tarefa foi concluída ou não).

A classe `ListaTarefas` possui um array `tarefas` para armazenar as tarefas e uma variável `proximoId` para controlar o próximo ID a ser atribuído a uma nova tarefa.

O exemplo também demonstra o uso da classe `ListaTarefas`, onde é criada uma instância da classe e são realizadas operações como adicionar tarefas, marcar como concluída, remover tarefas e listar todas as tarefas.

Espero que este exemplo atenda às suas expectativas!