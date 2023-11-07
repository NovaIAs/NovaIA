Claro, vou criar um exemplo de código TypeScript complexo que envolve a criação de uma aplicação de gerenciamento de tarefas em um contexto empresarial fictício. Este código será extenso e comentado para explicar sua funcionalidade em detalhes.

```typescript
// Definição de uma classe para representar uma tarefa
class Tarefa {
  constructor(public id: number, public descricao: string, public concluida: boolean) {}
}

// Classe que representa uma lista de tarefas
class ListaDeTarefas {
  private tarefas: Tarefa[] = [];

  // Método para adicionar uma nova tarefa
  adicionarTarefa(descricao: string) {
    const novaTarefa = new Tarefa(this.tarefas.length + 1, descricao, false);
    this.tarefas.push(novaTarefa);
  }

  // Método para marcar uma tarefa como concluída
  marcarTarefaConcluida(id: number) {
    const tarefa = this.tarefas.find((t) => t.id === id);
    if (tarefa) {
      tarefa.concluida = true;
    }
  }

  // Método para listar todas as tarefas
  listarTarefas() {
    console.log("Lista de Tarefas:");
    this.tarefas.forEach((t) => {
      const status = t.concluida ? "Concluída" : "Pendente";
      console.log(`ID: ${t.id}, Descrição: ${t.descricao}, Status: ${status}`);
    });
  }
}

// Exemplo de uso da aplicação
const lista = new ListaDeTarefas();
lista.adicionarTarefa("Fazer apresentação para reunião");
lista.adicionarTarefa("Preparar relatório financeiro");
lista.adicionarTarefa("Enviar e-mails de acompanhamento");

lista.listarTarefas();

lista.marcarTarefaConcluida(1);
lista.listarTarefas();

// Saída esperada:
// Lista de Tarefas:
// ID: 1, Descrição: Fazer apresentação para reunião, Status: Pendente
// ID: 2, Descrição: Preparar relatório financeiro, Status: Pendente
// ID: 3, Descrição:Enviar e-mails de acompanhamento, Status: Pendente
//
// Lista de Tarefas:
// ID: 1, Descrição: Fazer apresentação para reunião, Status: Concluída
// ID: 2, Descrição: Preparar relatório financeiro, Status: Pendente
// ID: 3, Descrição: Enviar e-mails de acompanhamento, Status: Pendente
```

Neste exemplo, criamos classes TypeScript para representar tarefas e uma lista de tarefas. A aplicação permite adicionar tarefas, marcar tarefas como concluídas e listar todas as tarefas. Comentários explicam cada parte do código em detalhes. Observe que este é apenas um exemplo, e a complexidade pode variar dependendo dos requisitos do projeto real.