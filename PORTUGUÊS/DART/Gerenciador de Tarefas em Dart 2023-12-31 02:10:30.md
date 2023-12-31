Claro! Aqui está um código complexo em Dart que implementa uma aplicação de gerenciamento de tarefas:

```dart
import 'dart:io';

class Tarefa {
  String nome;
  bool concluida;

  Tarefa(this.nome, this.concluida);
}

class GerenciadorTarefas {
  List<Tarefa> _tarefas = [];

  void adicionarTarefa(String nome) {
    Tarefa novaTarefa = Tarefa(nome, false);
    _tarefas.add(novaTarefa);
  }

  void marcarTarefaConcluida(String nome) {
    Tarefa? tarefa = _encontrarTarefa(nome);
    if (tarefa != null) {
      tarefa.concluida = true;
    }
  }

  void removerTarefa(String nome) {
    Tarefa? tarefa = _encontrarTarefa(nome);
    if (tarefa != null) {
      _tarefas.remove(tarefa);
    }
  }

  void listarTarefas() {
    for (int i = 0; i < _tarefas.length; i++) {
      Tarefa tarefa = _tarefas[i];
      String status = tarefa.concluida ? 'Concluída' : 'Pendente';
      print('${i + 1}. ${tarefa.nome} - $status');
    }
  }

  Tarefa? _encontrarTarefa(String nome) {
    for (Tarefa tarefa in _tarefas) {
      if (tarefa.nome == nome) {
        return tarefa;
      }
    }
    return null;
  }
}

void main() {
  GerenciadorTarefas gerenciador = GerenciadorTarefas();

  while (true) {
    print('===== GERENCIADOR DE TAREFAS =====');
    print('Escolha uma opção:');
    print('1. Adicionar tarefa');
    print('2. Marcar tarefa como concluída');
    print('3. Remover tarefa');
    print('4. Listar tarefas');
    print('5. Sair');

    String? opcao = stdin.readLineSync();

    if (opcao == '1') {
      print('Digite o nome da tarefa:');
      String? nomeTarefa = stdin.readLineSync();
      if (nomeTarefa != null) {
        gerenciador.adicionarTarefa(nomeTarefa);
        print('Tarefa adicionada com sucesso!');
      }
    } else if (opcao == '2') {
      gerenciador.listarTarefas();
      print('Digite o número da tarefa que deseja marcar como concluída:');
      String? numeroTarefa = stdin.readLineSync();
      if (numeroTarefa != null) {
        int index = int.parse(numeroTarefa) - 1;
        if (index >= 0 && index < gerenciador._tarefas.length) {
          Tarefa tarefa = gerenciador._tarefas[index];
          gerenciador.marcarTarefaConcluida(tarefa.nome);
          print('Tarefa marcada como concluída com sucesso!');
        } else {
          print('Número de tarefa inválido!');
        }
      }
    } else if (opcao == '3') {
      gerenciador.listarTarefas();
      print('Digite o número da tarefa que deseja remover:');
      String? numeroTarefa = stdin.readLineSync();
      if (numeroTarefa != null) {
        int index = int.parse(numeroTarefa) - 1;
        if (index >= 0 && index < gerenciador._tarefas.length) {
          Tarefa tarefa = gerenciador._tarefas[index];
          gerenciador.removerTarefa(tarefa.nome);
          print('Tarefa removida com sucesso!');
        } else {
          print('Número de tarefa inválido!');
        }
      }
    } else if (opcao == '4') {
      gerenciador.listarTarefas();
    } else if (opcao == '5') {
      print('Saindo do gerenciador de tarefas...');
      break;
    } else {
      print('Opção inválida!');
    }

    print('\n');
  }
}
```

Este código implementa um gerenciador de tarefas. Ele possui uma classe `Tarefa` que representa uma tarefa com um nome e um status de conclusão. A classe `GerenciadorTarefas` é responsável por adicionar, marcar como concluída, remover e listar as tarefas.

No método `main()`, um loop é executado para exibir um menu ao usuário e processar suas opções. O usuário pode adicionar uma tarefa, marcar uma tarefa como concluída, remover uma tarefa, listar as tarefas ou sair do programa.

A interação com o usuário é feita através da leitura de linhas do console usando `stdin.readLineSync()`. As tarefas são armazenadas em uma lista `_tarefas` privada na classe `GerenciadorTarefas`, e métodos são usados para realizar as operações desejadas.

Espero que este código complexo em Dart atenda às suas expectativas!