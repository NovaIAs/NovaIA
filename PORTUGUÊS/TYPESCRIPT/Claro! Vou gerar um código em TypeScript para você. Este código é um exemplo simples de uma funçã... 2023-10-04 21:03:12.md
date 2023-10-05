Claro! Vou gerar um código complexo em TypeScript para você. Este código é um exemplo de uma aplicação de lista de tarefas (to-do list) com funcionalidades avançadas, como a possibilidade de adicionar tarefas, marcar como concluídas, filtrar por status e remover tarefas.

```typescript
// Definindo uma interface para as tarefas
interface Tarefa {
  id: number;
  descricao: string;
  concluida: boolean;
}

// Classe responsável por gerenciar as tarefas
class GerenciadorTarefas {
  private tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  // Adicionar uma nova tarefa
  adicionarTarefa(descricao: string) {
    const id = this.tarefas.length + 1;
    const novaTarefa: Tarefa = {
      id,
      descricao,
      concluida: false,
    };
    this.tarefas.push(novaTarefa);
    console.log(`Tarefa adicionada: ${descricao}`);
  }

  // Marcar uma tarefa como concluída
  marcarComoConcluida(id: number) {
    const tarefa = this.buscarTarefaPorId(id);
    if (tarefa) {
      tarefa.concluida = true;
      console.log(`Tarefa ${tarefa.descricao} marcada como concluída`);
    } else {
      console.log(`Tarefa com o ID ${id} não encontrada`);
    }
  }

  // Remover uma tarefa
  removerTarefa(id: number) {
    const indice = this.buscarIndiceTarefaPorId(id);
    if (indice !== -1) {
      const tarefaRemovida = this.tarefas.splice(indice, 1)[0];
      console.log(`Tarefa ${tarefaRemovida.descricao} removida`);
    } else {
      console.log(`Tarefa com o ID ${id} não encontrada`);
    }
  }

  // Filtrar tarefas por status (concluídas ou não concluídas)
  filtrarPorStatus(concluida: boolean) {
    const tarefasFiltradas = this.tarefas.filter(tarefa => tarefa.concluida === concluida);
    console.log(`Tarefas ${concluida ? 'concluídas' : 'pendentes'}:`);
    tarefasFiltradas.forEach(tarefa => console.log(`- ${tarefa.descricao}`));
  }

  // Buscar uma tarefa pelo ID
  private buscarTarefaPorId(id: number): Tarefa | undefined {
    return this.tarefas.find(tarefa => tarefa.id === id);
  }

  // Buscar o índice de uma tarefa pelo ID
  private buscarIndiceTarefaPorId(id: number): number {
    return this.tarefas.findIndex(tarefa => tarefa.id === id);
  }
}

// Exemplo de uso
const gerenciador = new GerenciadorTarefas();

gerenciador.adicionarTarefa("Comprar leite");
gerenciador.adicionarTarefa("Pagar contas");
gerenciador.adicionarTarefa("Estudar TypeScript");

gerenciador.marcarComoConcluida(2);

gerenciador.filtrarPorStatus(false);

gerenciador.removerTarefa(3);
```

Neste código em TypeScript, temos uma classe chamada `GerenciadorTarefas` que é responsável por gerenciar as tarefas. A classe possui métodos para adicionar uma nova tarefa, marcar uma tarefa como concluída, remover uma tarefa e filtrar as tarefas por status.

A classe utiliza uma interface `Tarefa` para definir a estrutura de uma tarefa, com propriedades como `id`, `descricao` e `concluida`. O gerenciador de tarefas mantém um array de tarefas como estado interno.

No exemplo de uso, criamos uma instância do `GerenciadorTarefas`, adicionamos algumas tarefas, marcamos uma tarefa como concluída, filtramos as tarefas pendentes e removemos uma tarefa.

Espero que este código atenda às suas expectativas!