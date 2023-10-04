Claro! Vou te fornecer um exemplo de um código complexo em TypeScript que cria uma aplicação de lista de tarefas. A aplicação permite adicionar, remover e marcar tarefas como concluídas.

```typescript
class Tarefa {
  descricao: string;
  concluida: boolean;

  constructor(descricao: string) {
    this.descricao = descricao;
    this.concluida = false;
  }
}

class ListaDeTarefas {
  tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  adicionarTarefa(descricao: string): void {
    const tarefa = new Tarefa(descricao);
    this.tarefas.push(tarefa);
  }

  removerTarefa(index: number): void {
    if (index >= 0 && index < this.tarefas.length) {
      this.tarefas.splice(index, 1);
    }
  }

  marcarTarefaConcluida(index: number): void {
    if (index >= 0 && index < this.tarefas.length) {
      this.tarefas[index].concluida = true;
    }
  }

  listarTarefas(): void {
    console.log("Lista de Tarefas:");
    this.tarefas.forEach((tarefa, index) => {
      const status = tarefa.concluida ? "Concluída" : "Pendente";
      console.log(`${index + 1}. ${tarefa.descricao} - ${status}`);
    });
  }
}

// Uso da aplicação
const listaDeTarefas = new ListaDeTarefas();
listaDeTarefas.adicionarTarefa("Estudar TypeScript");
listaDeTarefas.adicionarTarefa("Fazer exercícios de programação");
listaDeTarefas.adicionarTarefa("Fazer compras");

listaDeTarefas.listarTarefas(); // Mostra todas as tarefas pendentes

listaDeTarefas.marcarTarefaConcluida(1); // Marca a segunda tarefa como concluída

listaDeTarefas.removerTarefa(2); // Remove a terceira tarefa

listaDeTarefas.listarTarefas(); // Mostra as tarefas restantes
```

Neste código, temos duas classes: `Tarefa` e `ListaDeTarefas`. A classe `Tarefa` define uma tarefa com uma descrição e um status de conclusão. A classe `ListaDeTarefas` gerencia uma lista de tarefas, permitindo adicionar, remover e marcar tarefas como concluídas.

Na parte de uso da aplicação, criamos uma instância da classe `ListaDeTarefas`, adicionamos algumas tarefas, mostramos a lista, marcamos uma tarefa como concluída, removemos outra tarefa e mostramos a lista atualizada.

Espero que esse exemplo atenda às suas expectativas! Caso precise de algo mais específico, é só me pedir.