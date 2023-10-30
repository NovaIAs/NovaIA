Claro! Aqui está um código complexo em TypeScript que cria uma aplicação de lista de tarefas com funcionalidades avançadas:

```typescript
// Definindo a classe para a Tarefa
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

  marcarIncompleta() {
    this.concluida = false;
  }
}

// Definindo a classe para a Lista de Tarefas
class ListaTarefas {
  tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  adicionarTarefa(descricao: string) {
    const novaTarefa = new Tarefa(descricao);
    this.tarefas.push(novaTarefa);
  }

  removerTarefa(index: number) {
    if (index >= 0 && index < this.tarefas.length) {
      this.tarefas.splice(index, 1);
    }
  }

  marcarTarefaConcluida(index: number) {
    if (index >= 0 && index < this.tarefas.length) {
      this.tarefas[index].marcarConcluida();
    }
  }

  marcarTarefaIncompleta(index: number) {
    if (index >= 0 && index < this.tarefas.length) {
      this.tarefas[index].marcarIncompleta();
    }
  }

  exibirTarefas() {
    console.log("Lista de Tarefas:");
    this.tarefas.forEach((tarefa, index) => {
      console.log(`${index + 1}. [${tarefa.concluida ? "x" : " "}] ${tarefa.descricao}`);
    });
  }
}

// Criando a instância da Lista de Tarefas
const listaTarefas = new ListaTarefas();

// Adicionando algumas tarefas
listaTarefas.adicionarTarefa("Estudar TypeScript");
listaTarefas.adicionarTarefa("Fazer compras");
listaTarefas.adicionarTarefa("Ler um livro");

// Marcar a primeira tarefa como concluída
listaTarefas.marcarTarefaConcluida(0);

// Remover a segunda tarefa
listaTarefas.removerTarefa(1);

// Exibir a lista de tarefas atualizada
listaTarefas.exibirTarefas();
```

Explicação do código:

Neste código, temos duas classes principais: `Tarefa` e `ListaTarefas`. A classe `Tarefa` representa uma única tarefa com atributos como `descricao` (descrição da tarefa) e `concluida` (indica se a tarefa está concluída ou não). A classe `ListaTarefas` representa a lista de tarefas, com métodos para adicionar, remover, marcar como concluída ou incompleta e exibir as tarefas.

Primeiro, criamos uma instância da `ListaTarefas` chamada `listaTarefas`. Em seguida, adicionamos algumas tarefas usando o método `adicionarTarefa`. Depois, marcamos a primeira tarefa como concluída usando o método `marcarTarefaConcluida`. Em seguida, removemos a segunda tarefa usando o método `removerTarefa`. Por fim, chamamos o método `exibirTarefas` para exibir a lista de tarefas atualizada.

O código é escrito em TypeScript, que é uma linguagem de programação tipada que se baseia no JavaScript. Ele fornece recursos adicionais, como verificação de tipo estática, classes e interfaces, o que torna o código mais robusto e seguro.