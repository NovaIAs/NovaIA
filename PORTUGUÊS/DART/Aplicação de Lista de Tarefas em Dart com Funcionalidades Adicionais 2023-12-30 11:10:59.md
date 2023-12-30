Claro! Aqui está um exemplo de um código complexo em Dart, que consiste em uma aplicação de lista de tarefas com funcionalidades adicionais:

```dart
import 'dart:io';

List<String> tarefas = [];
List<String> tarefasConcluidas = [];

void main() {
  bool executando = true;

  while (executando) {
    print('Escolha uma opção:');
    print('1 - Adicionar tarefa');
    print('2 - Concluir tarefa');
    print('3 - Exibir tarefas');
    print('4 - Exibir tarefas concluídas');
    print('5 - Sair');

    String opcao = stdin.readLineSync()!;

    switch (opcao) {
      case '1':
        adicionarTarefa();
        break;
      case '2':
        concluirTarefa();
        break;
      case '3':
        exibirTarefas();
        break;
      case '4':
        exibirTarefasConcluidas();
        break;
      case '5':
        executando = false;
        break;
      default:
        print('Opção inválida.');
        break;
    }
  }
}

void adicionarTarefa() {
  print('Digite a tarefa:');
  String novaTarefa = stdin.readLineSync()!;
  tarefas.add(novaTarefa);
  print('Tarefa adicionada com sucesso!');
}

void concluirTarefa() {
  print('Digite o número da tarefa que deseja concluir:');
  exibirTarefas();

  int numeroTarefa = int.parse(stdin.readLineSync()!);
  if (numeroTarefa >= 1 && numeroTarefa <= tarefas.length) {
    String tarefaConcluida = tarefas.removeAt(numeroTarefa - 1);
    tarefasConcluidas.add(tarefaConcluida);
    print('Tarefa concluída com sucesso!');
  } else {
    print('Número de tarefa inválido.');
  }
}

void exibirTarefas() {
  if (tarefas.isEmpty) {
    print('Nenhuma tarefa na lista.');
  } else {
    print('Tarefas:');
    for (int i = 0; i < tarefas.length; i++) {
      print('${i + 1} - ${tarefas[i]}');
    }
  }
  print('\n');
}

void exibirTarefasConcluidas() {
  if (tarefasConcluidas.isEmpty) {
    print('Nenhuma tarefa concluída.');
  } else {
    print('Tarefas concluídas:');
    for (int i = 0; i < tarefasConcluidas.length; i++) {
      print('${i + 1} - ${tarefasConcluidas[i]}');
    }
  }
  print('\n');
}
```

Este código em Dart implementa uma aplicação de lista de tarefas com as seguintes funcionalidades:

1. Adicionar tarefa: Permite ao usuário adicionar uma nova tarefa à lista.
2. Concluir tarefa: Permite ao usuário marcar uma tarefa como concluída e movê-la para a lista de tarefas concluídas.
3. Exibir tarefas: Exibe todas as tarefas presentes na lista.
4. Exibir tarefas concluídas: Exibe todas as tarefas concluídas.
5. Sair: Encerra a aplicação.

A aplicação utiliza um loop `while` para apresentar continuamente o menu de opções ao usuário até que ele escolha a opção de sair. Cada opção é tratada em uma função separada, que executa a lógica correspondente.

Espero que este exemplo atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais alguma ajuda, fique à vontade para perguntar.