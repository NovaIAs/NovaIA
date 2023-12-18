Claro! Aqui está um exemplo de um código complexo em TypeScript que cria uma aplicação para gerenciar uma lista de tarefas:

```typescript
interface Tarefa {
  id: number;
  descricao: string;
  concluida: boolean;
}

class ListaTarefas {
  private tarefas: Tarefa[] = [];

  adicionarTarefa(descricao: string): void {
    const id = this.tarefas.length + 1;
    const novaTarefa: Tarefa = {
      id,
      descricao,
      concluida: false,
    };
    this.tarefas.push(novaTarefa);
  }

  marcarTarefaComoConcluida(id: number): void {
    const tarefa = this.buscarTarefaPorId(id);
    if (tarefa) {
      tarefa.concluida = true;
    }
  }

  removerTarefa(id: number): void {
    const indice = this.buscarIndiceTarefaPorId(id);
    if (indice !== -1) {
      this.tarefas.splice(indice, 1);
    }
  }

  listarTarefas(): void {
    console.log("Lista de Tarefas:");
    this.tarefas.forEach((tarefa) => {
      const status = tarefa.concluida ? "Concluída" : "Pendente";
      console.log(`ID: ${tarefa.id}, Descrição: ${tarefa.descricao}, Status: ${status}`);
    });
  }

  private buscarTarefaPorId(id: number): Tarefa | undefined {
    return this.tarefas.find((tarefa) => tarefa.id === id);
  }

  private buscarIndiceTarefaPorId(id: number): number {
    return this.tarefas.findIndex((tarefa) => tarefa.id === id);
  }
}

const lista = new ListaTarefas();
lista.adicionarTarefa("Estudar TypeScript");
lista.adicionarTarefa("Fazer compras");
lista.adicionarTarefa("Lavar o carro");
lista.listarTarefas();
lista.marcarTarefaComoConcluida(2);
lista.removerTarefa(3);
lista.listarTarefas();
```

Explicação:

1. Primeiramente, criamos uma interface chamada `Tarefa`, que define a estrutura de cada tarefa, com os campos `id`, `descricao` e `concluida`.
2. Em seguida, criamos a classe `ListaTarefas`, que contém um array privado de tarefas.
3. A classe possui métodos para adicionar uma nova tarefa, marcar uma tarefa como concluída, remover uma tarefa e listar todas as tarefas.
4. O método `adicionarTarefa` recebe a descrição da tarefa, cria um novo objeto `Tarefa` com um novo ID e adiciona à lista de tarefas.
5. O método `marcarTarefaComoConcluida` recebe o ID de uma tarefa e marca-a como concluída, alterando o valor do campo `concluida` para `true`.
6. O método `removerTarefa` recebe o ID de uma tarefa e remove-a da lista de tarefas.
7. O método `listarTarefas` percorre todas as tarefas da lista e exibe na console o ID, descrição e status de cada uma.
8. Os métodos `buscarTarefaPorId` e `buscarIndiceTarefaPorId` são métodos auxiliares privados para buscar uma tarefa pelo ID e retornar a tarefa ou o índice da tarefa na lista, respectivamente.
9. Por fim, criamos uma instância da classe `ListaTarefas`, adicionamos algumas tarefas, listamos as tarefas, marcamos a segunda tarefa como concluída, removemos a terceira tarefa e listamos novamente as tarefas para verificar as modificações.

Este código é apenas um exemplo complexo de uma aplicação de gerenciamento de tarefas em TypeScript. Pode ser expandido e personalizado de acordo com as necessidades específicas do projeto.