Claro, vou criar um código complexo em TypeScript para você. Este código é um exemplo de um sistema de gerenciamento de tarefas. Ele permite adicionar, listar, atualizar e excluir tarefas.

```typescript
interface Tarefa {
  id: number;
  titulo: string;
  descricao: string;
  concluida: boolean;
}

class GerenciadorTarefas {
  private tarefas: Tarefa[];

  constructor() {
    this.tarefas = [];
  }

  adicionarTarefa(titulo: string, descricao: string): void {
    const id = this.tarefas.length + 1;
    const tarefa: Tarefa = {
      id,
      titulo,
      descricao,
      concluida: false,
    };
    this.tarefas.push(tarefa);
    console.log(`Tarefa com ID ${id} adicionada.`);
  }

  listarTarefas(): void {
    console.log("Lista de tarefas:");
    if (this.tarefas.length === 0) {
      console.log("Nenhuma tarefa encontrada.");
    } else {
      this.tarefas.forEach((tarefa) => {
        const status = tarefa.concluida ? "Concluída" : "Pendente";
        console.log(
          `ID: ${tarefa.id}, Título: ${tarefa.titulo}, Descrição: ${tarefa.descricao}, Status: ${status}`
        );
      });
    }
  }

  atualizarTarefa(id: number, titulo: string, descricao: string): void {
    const tarefa = this.tarefas.find((t) => t.id === id);
    if (tarefa) {
      tarefa.titulo = titulo;
      tarefa.descricao = descricao;
      console.log(`Tarefa com ID ${id} atualizada.`);
    } else {
      console.log(`Tarefa com ID ${id} não encontrada.`);
    }
  }

  excluirTarefa(id: number): void {
    const index = this.tarefas.findIndex((t) => t.id === id);
    if (index !== -1) {
      this.tarefas.splice(index, 1);
      console.log(`Tarefa com ID ${id} excluída.`);
    } else {
      console.log(`Tarefa com ID ${id} não encontrada.`);
    }
  }
}

const gerenciador = new GerenciadorTarefas();
gerenciador.adicionarTarefa("Estudar TypeScript", "Estudar os conceitos básicos do TypeScript.");
gerenciador.adicionarTarefa("Fazer exercícios", "Resolver os exercícios de programação.");
gerenciador.listarTarefas();
gerenciador.atualizarTarefa(1, "Estudar JavaScript", "Estudar os conceitos básicos do JavaScript.");
gerenciador.listarTarefas();
gerenciador.excluirTarefa(2);
gerenciador.listarTarefas();
```

Neste código, temos a definição da interface `Tarefa`, que representa a estrutura de uma tarefa com os campos `id`, `titulo`, `descricao` e `concluida` (indicando se a tarefa está concluída ou não).

A classe `GerenciadorTarefas` é responsável por gerenciar as tarefas. Ela possui um array `tarefas` que armazena as tarefas adicionadas. No construtor, inicializamos o array vazio.

O método `adicionarTarefa` permite adicionar uma nova tarefa ao array `tarefas`. É gerado um novo ID com base no tamanho do array mais um, e então a tarefa é adicionada ao array.

O método `listarTarefas` exibe no console a lista de tarefas. Se o array está vazio, é exibida uma mensagem informando que nenhuma tarefa foi encontrada. Caso contrário, para cada tarefa no array, é exibido o ID, título, descrição e status (concluída ou pendente).

O método `atualizarTarefa` permite atualizar o título e a descrição de uma tarefa existente. Ele busca a tarefa pelo ID fornecido e, se encontrada, atualiza os campos.

O método `excluirTarefa` permite excluir uma tarefa pelo ID. Ele busca o índice da tarefa no array e, se encontrado, remove a tarefa utilizando o método `splice`.

No exemplo, é criada uma instância do `GerenciadorTarefas`, algumas tarefas são adicionadas, a lista é exibida, uma tarefa é atualizada, a lista é exibida novamente e, por fim, uma tarefa é excluída e a lista é exibida mais uma vez.