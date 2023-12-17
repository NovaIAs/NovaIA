Claro! Aqui está um exemplo de código complexo em TypeScript, que cria uma aplicação de gerenciamento de tarefas:

```typescript
class Tarefa {
  descricao: string;
  concluida: boolean;

  constructor(descricao: string) {
    this.descricao = descricao;
    this.concluida = false;
  }

  marcarConcluida() {
    this.concluida = true;
  }

  marcarPendente() {
    this.concluida = false;
  }
}

class ListaTarefas {
  tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  adicionarTarefa(descricao: string) {
    const novaTarefa = new Tarefa(descricao);
    this.tarefas.push(novaTarefa);
  }

  removerTarefa(descricao: string) {
    const indice = this.tarefas.findIndex(tarefa => tarefa.descricao === descricao);
    if (indice !== -1) {
      this.tarefas.splice(indice, 1);
    }
  }

  marcarTarefaConcluida(descricao: string) {
    const tarefa = this.tarefas.find(tarefa => tarefa.descricao === descricao);
    if (tarefa) {
      tarefa.marcarConcluida();
    }
  }

  marcarTarefaPendente(descricao: string) {
    const tarefa = this.tarefas.find(tarefa => tarefa.descricao === descricao);
    if (tarefa) {
      tarefa.marcarPendente();
    }
  }

  listarTarefas() {
    this.tarefas.forEach(tarefa => {
      const status = tarefa.concluida ? 'Concluída' : 'Pendente';
      console.log(`Descrição: ${tarefa.descricao} | Status: ${status}`);
    });
  }
}

const lista = new ListaTarefas();
lista.adicionarTarefa('Estudar TypeScript');
lista.adicionarTarefa('Fazer exercícios de programação');
lista.adicionarTarefa('Ler livro de desenvolvimento pessoal');

lista.marcarTarefaConcluida('Fazer exercícios de programação');

lista.listarTarefas();
```

Neste código, utilizamos classes para representar tarefas individuais e uma lista de tarefas. A classe `Tarefa` possui propriedades como `descricao` (que armazena a descrição da tarefa) e `concluida` (que indica se a tarefa foi concluída ou não). A classe também possui métodos para marcar a tarefa como concluída ou pendente.

A classe `ListaTarefas` é responsável por gerenciar a lista de tarefas. Ela possui métodos para adicionar e remover tarefas, além de marcar uma tarefa como concluída ou pendente. O método `listarTarefas` exibe todas as tarefas da lista, juntamente com seu status.

No exemplo final, criamos uma instância da classe `ListaTarefas`, adicionamos algumas tarefas, marcamos uma delas como concluída e então exibimos todas as tarefas utilizando o método `listarTarefas`.

Espero que este exemplo atenda às suas expectativas!