Claro! Aqui está um exemplo de código TypeScript que cria uma aplicação de gerenciamento de tarefas usando o conceito de Programação Orientada a Objetos (POO). Este código utiliza classes, interfaces, herança e genéricos para criar uma estrutura de dados flexível para gerenciar tarefas.

```typescript
interface Tarefa {
  nome: string;
  concluida: boolean;
  prioridade: number;
}

class ListaTarefas<T extends Tarefa> {
  private lista: T[];

  constructor() {
    this.lista = [];
  }

  adicionar(tarefa: T): void {
    this.lista.push(tarefa);
  }

  remover(nomeTarefa: string): void {
    const indice = this.lista.findIndex((tarefa) => tarefa.nome === nomeTarefa);
    if (indice !== -1) {
      this.lista.splice(indice, 1);
    }
  }

  listar(): void {
    console.log("Tarefas:");
    this.lista.forEach((tarefa) => {
      console.log(`- ${tarefa.nome} (${tarefa.concluida ? 'Concluída' : 'Pendente'}) - Prioridade: ${tarefa.prioridade}`);
    });
  }

  concluir(nomeTarefa: string): void {
    const tarefa = this.lista.find((tarefa) => tarefa.nome === nomeTarefa);
    if (tarefa) {
      tarefa.concluida = true;
    }
  }
}

class TarefaPersonalizada implements Tarefa {
  nome: string;
  concluida: boolean;
  prioridade: number;
  descricao: string;

  constructor(nome: string, prioridade: number, descricao: string) {
    this.nome = nome;
    this.concluida = false;
    this.prioridade = prioridade;
    this.descricao = descricao;
  }
}

class TarefaSimples implements Tarefa {
  nome: string;
  concluida: boolean;
  prioridade: number;

  constructor(nome: string, prioridade: number) {
    this.nome = nome;
    this.concluida = false;
    this.prioridade = prioridade;
  }
}

const lista = new ListaTarefas<Tarefa>();
const tarefa1 = new TarefaPersonalizada("Comprar mantimentos", 1, "Ir ao supermercado e comprar itens básicos.");
const tarefa2 = new TarefaSimples("Estudar TypeScript", 2);
const tarefa3 = new TarefaSimples("Fazer exercícios", 3);

lista.adicionar(tarefa1);
lista.adicionar(tarefa2);
lista.adicionar(tarefa3);

lista.listar();

lista.concluir("Estudar TypeScript");

lista.listar();

lista.remover("Fazer exercícios");

lista.listar();
```

Neste código, temos a definição de uma interface `Tarefa` que representa as propriedades básicas de uma tarefa, como nome, status de conclusão e prioridade. Em seguida, temos a classe `ListaTarefas` que possui métodos para adicionar, remover, listar e concluir tarefas.

Além disso, temos duas classes que implementam a interface `Tarefa`: `TarefaPersonalizada` e `TarefaSimples`. A primeira classe inclui uma propriedade adicional `descricao`, que permite uma tarefa personalizada ter uma descrição mais detalhada.

O código cria uma instância da classe `ListaTarefas`, adiciona algumas tarefas personalizadas e tarefas simples, lista todas as tarefas, conclui uma tarefa específica, remove outra tarefa e, em seguida, lista as tarefas novamente para demonstrar as alterações feitas.

Este exemplo ilustra como é possível criar uma aplicação de gerenciamento de tarefas flexível e extensível usando TypeScript e conceitos de POO.