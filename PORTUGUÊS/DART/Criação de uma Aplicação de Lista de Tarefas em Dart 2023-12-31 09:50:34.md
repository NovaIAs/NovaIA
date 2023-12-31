Claro! Aqui está um código complexo em Dart que envolve a criação de uma aplicação de lista de tarefas com recursos de adicionar, remover e marcar tarefas como concluídas. Vou explicar o código linha por linha:

```dart
import 'dart:io';

void main() {
  var listaTarefas = <String>[]; // Cria uma lista vazia para armazenar as tarefas

  while (true) {
    print('\nO que você gostaria de fazer?');
    print('1 - Adicionar uma tarefa');
    print('2 - Remover uma tarefa');
    print('3 - Marcar uma tarefa como concluída');
    print('4 - Sair');

    var opcao = stdin.readLineSync(); // Lê a opção escolhida pelo usuário

    switch (opcao) {
      case '1':
        adicionarTarefa(listaTarefas); // Chama a função para adicionar uma tarefa
        break;
      case '2':
        removerTarefa(listaTarefas); // Chama a função para remover uma tarefa
        break;
      case '3':
        marcarTarefaConcluida(listaTarefas); // Chama a função para marcar uma tarefa como concluída
        break;
      case '4':
        print('Saindo...'); // Exibe uma mensagem de saída
        return;
      default:
        print('Opção inválida. Tente novamente.'); // Exibe uma mensagem de opção inválida
    }
  }
}

void adicionarTarefa(List<String> listaTarefas) {
  print('Digite a tarefa que deseja adicionar:');
  var tarefa = stdin.readLineSync(); // Lê a tarefa digitada pelo usuário
  listaTarefas.add(tarefa); // Adiciona a tarefa à lista
  print('Tarefa adicionada com sucesso!');
}

void removerTarefa(List<String> listaTarefas) {
  if (listaTarefas.isEmpty) {
    print('A lista de tarefas está vazia.');
    return;
  }

  print('Digite o número da tarefa que deseja remover:');
  listarTarefas(listaTarefas); // Chama a função para listar as tarefas
  var numeroTarefa = int.parse(stdin.readLineSync()); // Lê o número da tarefa a ser removida

  if (numeroTarefa >= 1 && numeroTarefa <= listaTarefas.length) {
    listaTarefas.removeAt(numeroTarefa - 1); // Remove a tarefa da lista
    print('Tarefa removida com sucesso!');
  } else {
    print('Número de tarefa inválido. Tente novamente.');
  }
}

void marcarTarefaConcluida(List<String> listaTarefas) {
  if (listaTarefas.isEmpty) {
    print('A lista de tarefas está vazia.');
    return;
  }

  print('Digite o número da tarefa que deseja marcar como concluída:');
  listarTarefas(listaTarefas); // Chama a função para listar as tarefas
  var numeroTarefa = int.parse(stdin.readLineSync()); // Lê o número da tarefa a ser marcada como concluída

  if (numeroTarefa >= 1 && numeroTarefa <= listaTarefas.length) {
    var tarefaConcluida = listaTarefas[numeroTarefa - 1];
    listaTarefas[numeroTarefa - 1] = '✔ $tarefaConcluida'; // Marca a tarefa como concluída adicionando um ✔ no início
    print('Tarefa marcada como concluída com sucesso!');
  } else {
    print('Número de tarefa inválido. Tente novamente.');
  }
}

void listarTarefas(List<String> listaTarefas) {
  for (var i = 0; i < listaTarefas.length; i++) {
    print('${i + 1} - ${listaTarefas[i]}'); // Exibe o número e o nome da tarefa
  }
}
```

Este código cria uma aplicação interativa de lista de tarefas no terminal, onde o usuário pode adicionar, remover e marcar tarefas como concluídas. A função `main` é responsável por exibir o menu de opções e chamar as funções correspondentes de acordo com a escolha do usuário.

As funções `adicionarTarefa`, `removerTarefa` e `marcarTarefaConcluida` executam as ações correspondentes e interagem com o usuário para coletar as informações necessárias.

A função `listarTarefas` é utilizada para exibir a lista de tarefas numeradas, facilitando a seleção de tarefas para remoção ou marcação como concluída.

Espero que este código atenda às suas especificações! Se tiver alguma dúvida ou precisar de mais informações, estou à disposição para ajudar.