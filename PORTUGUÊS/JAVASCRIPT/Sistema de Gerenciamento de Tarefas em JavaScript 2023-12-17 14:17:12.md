Claro! Vou gerar um código em JavaScript complexo e diferenciado para você. O código criado será um sistema de gerenciamento de tarefas. Neste sistema, você poderá adicionar, remover e listar tarefas, bem como marcar tarefas como concluídas. Vamos lá:

```javascript
// Definindo a classe Tarefa
class Tarefa {
  constructor(descricao, concluida) {
    this.descricao = descricao;
    this.concluida = concluida;
  }
}

// Definindo a classe GerenciadorTarefas
class GerenciadorTarefas {
  constructor() {
    this.tarefas = [];
  }

  adicionarTarefa(descricao) {
    const novaTarefa = new Tarefa(descricao, false);
    this.tarefas.push(novaTarefa);
  }

  removerTarefa(descricao) {
    const indice = this.tarefas.findIndex(tarefa => tarefa.descricao === descricao);
    if (indice !== -1) {
      this.tarefas.splice(indice, 1);
    }
  }

  marcarTarefaConcluida(descricao) {
    const tarefa = this.tarefas.find(tarefa => tarefa.descricao === descricao);
    if (tarefa) {
      tarefa.concluida = true;
    }
  }

  listarTarefas() {
    console.log("Lista de Tarefas:");
    this.tarefas.forEach((tarefa, indice) => {
      console.log(`${indice + 1}. ${tarefa.descricao} - ${tarefa.concluida ? "Concluída" : "Pendente"}`);
    });
  }
}

// Utilizando o sistema de gerenciamento de tarefas
const gerenciador = new GerenciadorTarefas();

gerenciador.adicionarTarefa("Estudar JavaScript");
gerenciador.adicionarTarefa("Fazer compras");
gerenciador.listarTarefas();

gerenciador.marcarTarefaConcluida("Estudar JavaScript");
gerenciador.listarTarefas();

gerenciador.removerTarefa("Fazer compras");
gerenciador.listarTarefas();
```

Neste código, temos duas classes: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` é responsável por representar uma tarefa, com uma descrição e um status de conclusão. A classe `GerenciadorTarefas` é responsável por gerenciar todas as tarefas, permitindo adicionar, remover, marcar como concluída e listar as tarefas.

No exemplo de uso do código, criamos uma instância do `GerenciadorTarefas`, adicionamos duas tarefas, listamos as tarefas, marcamos a primeira como concluída, listamos novamente e, por fim, removemos a segunda tarefa e listamos novamente.

Espero que este código complexo e diferenciado atenda às suas expectativas!