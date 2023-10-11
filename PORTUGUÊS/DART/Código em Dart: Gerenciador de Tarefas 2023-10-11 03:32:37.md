Claro! Aqui está um código complexo em Dart que envolve várias funcionalidades e abordagens diferentes. Este código é um exemplo de um aplicativo de gerenciamento de tarefas que permite adicionar, remover e marcar tarefas como concluídas.

```dart
import 'dart:io';

class Task {
  String name;
  bool isCompleted;

  Task(this.name, {this.isCompleted = false});

  @override
  String toString() {
    return '${isCompleted ? '[X]' : '[ ]'} $name';
  }
}

void main() {
  List<Task> tasks = [];

  while (true) {
    print('===== Gerenciador de Tarefas =====');
    print('1. Adicionar tarefa');
    print('2. Remover tarefa');
    print('3. Marcar tarefa como concluída');
    print('4. Listar tarefas');
    print('5. Sair');
    stdout.write('Escolha uma opção (1-5): ');

    int option = int.parse(stdin.readLineSync()!);
    print('');

    switch (option) {
      case 1:
        stdout.write('Digite o nome da tarefa: ');
        String taskName = stdin.readLineSync()!;
        tasks.add(Task(taskName));
        print('Tarefa adicionada com sucesso!\n');
        break;

      case 2:
        stdout.write('Digite o índice da tarefa a ser removida: ');
        int index = int.parse(stdin.readLineSync()!);
        if (index >= 0 && index < tasks.length) {
          Task removedTask = tasks.removeAt(index);
          print('Tarefa "${removedTask.name}" removida com sucesso!\n');
        } else {
          print('Índice inválido!\n');
        }
        break;

      case 3:
        stdout.write('Digite o índice da tarefa a ser marcada como concluída: ');
        int index = int.parse(stdin.readLineSync()!);
        if (index >= 0 && index < tasks.length) {
          tasks[index].isCompleted = true;
          print('Tarefa marcada como concluída!\n');
        } else {
          print('Índice inválido!\n');
        }
        break;

      case 4:
        print('===== Tarefas =====');
        if (tasks.isEmpty) {
          print('Nenhuma tarefa adicionada ainda.\n');
        } else {
          for (int i = 0; i < tasks.length; i++) {
            print('$i. ${tasks[i]}');
          }
          print('');
        }
        break;

      case 5:
        print('Saindo do programa...');
        return;

      default:
        print('Opção inválida!\n');
        break;
    }
  }
}
```

Este código começa importando a biblioteca `dart:io` para permitir a interação com o terminal. Em seguida, declaramos uma classe `Task` que representa uma tarefa com um nome e um indicador de conclusão.

Dentro da função `main()`, criamos uma lista vazia de tarefas `tasks`. Em um loop infinito, exibimos um menu com diferentes opções para o usuário. Dependendo da opção escolhida, o código executa diferentes ações:

1. Adicionar tarefa: Solicita ao usuário o nome da tarefa e cria um objeto `Task` com o nome fornecido. Em seguida, adiciona a tarefa à lista `tasks`.

2. Remover tarefa: Solicita ao usuário o índice da tarefa a ser removida e, se o índice for válido, remove a tarefa correspondente da lista `tasks`.

3. Marcar tarefa como concluída: Solicita ao usuário o índice da tarefa a ser marcada como concluída e, se o índice for válido, atualiza o indicador de conclusão da tarefa correspondente para `true`.

4. Listar tarefas: Exibe todas as tarefas da lista `tasks` com seus respectivos índices e estado de conclusão.

5. Sair: Encerra o programa.

Se o usuário escolher uma opção inválida, uma mensagem de erro será exibida.

Este código é uma implementação simples de um aplicativo de gerenciamento de tarefas em Dart. Você pode usá-lo como ponto de partida e adicionar mais recursos e funcionalidades conforme necessário.