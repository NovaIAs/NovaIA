Claro! Vou gerar um código complexo em Dart que vai te surpreender. Aqui está um exemplo de um código que implementa um sistema de gerenciamento de tarefas:

```dart
import 'dart:io';

class Tarefa {
  String descricao;
  bool concluida;

  Tarefa(this.descricao, this.concluida);
}

class GerenciadorTarefas {
  List<Tarefa> tarefas = [];

  void adicionarTarefa(String descricao) {
    tarefas.add(Tarefa(descricao, false));
    print('Tarefa adicionada com sucesso!');
  }

  void listarTarefas() {
    if (tarefas.isEmpty) {
      print('Não há tarefas cadastradas.');
    } else {
      for (var i = 0; i < tarefas.length; i++) {
        var tarefa = tarefas[i];
        var status = tarefa.concluida ? 'Concluída' : 'Pendente';
        print('${i + 1}. ${tarefa.descricao} - $status');
      }
    }
  }

  void marcarTarefaConcluida(int indice) {
    if (indice < 1 || indice > tarefas.length) {
      print('Índice inválido!');
    } else {
      var tarefa = tarefas[indice - 1];
      if (tarefa.concluida) {
        print('Essa tarefa já foi concluída!');
      } else {
        tarefa.concluida = true;
        print('Tarefa marcada como concluída com sucesso!');
      }
    }
  }

  void removerTarefa(int indice) {
    if (indice < 1 || indice > tarefas.length) {
      print('Índice inválido!');
    } else {
      tarefas.removeAt(indice - 1);
      print('Tarefa removida com sucesso!');
    }
  }
}

void main() {
  var gerenciador = GerenciadorTarefas();

  while (true) {
    print('----- MENU -----');
    print('1. Adicionar tarefa');
    print('2. Listar tarefas');
    print('3. Marcar tarefa como concluída');
    print('4. Remover tarefa');
    print('5. Sair');
    stdout.write('Escolha uma opção: ');
    var opcao = int.parse(stdin.readLineSync());

    if (opcao == 1) {
      stdout.write('Descrição da tarefa: ');
      var descricao = stdin.readLineSync();
      gerenciador.adicionarTarefa(descricao);
    } else if (opcao == 2) {
      gerenciador.listarTarefas();
    } else if (opcao == 3) {
      gerenciador.listarTarefas();
      stdout.write('Digite o número da tarefa que deseja marcar como concluída: ');
      var indice = int.parse(stdin.readLineSync());
      gerenciador.marcarTarefaConcluida(indice);
    } else if (opcao == 4) {
      gerenciador.listarTarefas();
      stdout.write('Digite o número da tarefa que deseja remover: ');
      var indice = int.parse(stdin.readLineSync());
      gerenciador.removerTarefa(indice);
    } else if (opcao == 5) {
      break;
    } else {
      print('Opção inválida!');
    }

    print('\n');
  }
}
```

Neste código, criamos duas classes: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa com uma descrição e um status (concluída ou pendente). A classe `GerenciadorTarefas` é responsável por adicionar, listar, marcar como concluída e remover tarefas.

No método `main`, criamos um menu interativo que permite ao usuário escolher as opções de adicionar tarefa, listar tarefas, marcar como concluída e remover tarefa. O loop principal garante que o menu seja exibido continuamente até que o usuário escolha a opção de sair.

Espero que este código atenda às suas expectativas!