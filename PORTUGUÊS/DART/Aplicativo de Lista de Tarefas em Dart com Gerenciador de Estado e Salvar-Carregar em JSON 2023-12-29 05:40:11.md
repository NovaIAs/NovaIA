Claro, vou criar um código complexo em Dart para você. Este código é um exemplo de uma aplicação de lista de tarefas com algumas funcionalidades adicionais. Ele contém um gerenciador de estado, permite adicionar, remover e editar tarefas, além de salvar e carregar tarefas em um arquivo JSON. Vou explicar cada parte do código para você:

```dart
import 'dart:io';
import 'dart:convert';

void main() {
  var taskList = TaskList(); // Cria uma nova instância da lista de tarefas
  taskList.loadTasks(); // Carrega as tarefas salvas, se houver

  while (true) {
    print('O que você deseja fazer?');
    print('(1) Adicionar uma tarefa');
    print('(2) Remover uma tarefa');
    print('(3) Editar uma tarefa');
    print('(4) Mostrar todas as tarefas');
    print('(5) Sair');

    var option = int.parse(stdin.readLineSync()); // Lê a opção escolhida pelo usuário

    switch (option) {
      case 1:
        addTask(taskList);
        break;
      case 2:
        removeTask(taskList);
        break;
      case 3:
        editTask(taskList);
        break;
      case 4:
        showTasks(taskList);
        break;
      case 5:
        taskList.saveTasks(); // Salva as tarefas antes de sair
        exit(0);
        break;
      default:
        print('Opção inválida');
    }
  }
}

class Task {
  String description;
  bool completed;

  Task(this.description, this.completed);
}

class TaskList {
  List<Task> tasks = [];

  void loadTasks() {
    try {
      var file = File('tasks.json');
      if (file.existsSync()) {
        var json = file.readAsStringSync();
        var jsonData = jsonDecode(json);
        tasks = jsonData.map<Task>((task) {
          return Task(task['description'], task['completed']);
        }).toList();
      }
    } catch (e) {
      print('Erro ao carregar as tarefas: $e');
    }
  }

  void saveTasks() {
    try {
      var file = File('tasks.json');
      var jsonData = tasks.map<Map<String, dynamic>>((task) {
        return {
          'description': task.description,
          'completed': task.completed,
        };
      }).toList();
      var json = jsonEncode(jsonData);
      file.writeAsStringSync(json);
    } catch (e) {
      print('Erro ao salvar as tarefas: $e');
    }
  }

  void addTask() {
    print('Digite a descrição da tarefa:');
    var description = stdin.readLineSync();
    tasks.add(Task(description, false));
    print('Tarefa adicionada com sucesso!');
  }

  void removeTask() {
    showTasks();
    print('Digite o número da tarefa que deseja remover:');
    var index = int.parse(stdin.readLineSync());
    if (index >= 0 && index < tasks.length) {
      tasks.removeAt(index);
      print('Tarefa removida com sucesso!');
    } else {
      print('Tarefa não encontrada');
    }
  }

  void editTask() {
    showTasks();
    print('Digite o número da tarefa que deseja editar:');
    var index = int.parse(stdin.readLineSync());
    if (index >= 0 && index < tasks.length) {
      print('Digite a nova descrição da tarefa:');
      var description = stdin.readLineSync();
      tasks[index].description = description;
      print('Tarefa editada com sucesso!');
    } else {
      print('Tarefa não encontrada');
    }
  }

  void showTasks() {
    if (tasks.isEmpty) {
      print('Nenhuma tarefa encontrada');
    } else {
      for (var i = 0; i < tasks.length; i++) {
        var task = tasks[i];
        var status = task.completed ? 'Concluída' : 'Pendente';
        print('$i - ${task.description} ($status)');
      }
    }
  }
}
```

Este código utiliza a linguagem Dart para criar uma lista de tarefas com funcionalidades básicas. Ele permite ao usuário adicionar, remover, editar e mostrar tarefas. Além disso, as tarefas são salvas e carregadas de um arquivo JSON.

A classe `Task` representa uma tarefa com uma descrição e um estado (concluída ou pendente). A classe `TaskList` é responsável por gerenciar a lista de tarefas. Ela possui métodos para carregar as tarefas do arquivo JSON, salvar as tarefas no arquivo JSON, adicionar tarefas, remover tarefas, editar tarefas, e mostrar todas as tarefas.

No método `main()`, o programa apresenta um menu com as opções disponíveis e aguarda a entrada do usuário. Dependendo da opção escolhida, o programa chama os métodos adequados da classe `TaskList`. A opção 5 permite sair do programa e antes disso, as tarefas são salvas.

Espero que este código atenda às suas expectativas e seja útil para você!