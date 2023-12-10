Claro! Aqui está um exemplo de um código em Dart que cria uma lista de contatos, permitindo adicionar, remover e exibir os contatos:

```dart
import 'dart:io';

class Contato {
  String nome;
  String telefone;

  Contato({required this.nome, required this.telefone});
}

class ListaContatos {
  List<Contato> contatos = [];

  void adicionarContato() {
    stdout.write("Digite o nome do contato: ");
    var nome = stdin.readLineSync();
    stdout.write("Digite o telefone do contato: ");
    var telefone = stdin.readLineSync();

    var novoContato = Contato(nome: nome!, telefone: telefone!);
    contatos.add(novoContato);
    print("Contato adicionado com sucesso!");
  }

  void removerContato() {
    stdout.write("Digite o nome do contato que deseja remover: ");
    var nome = stdin.readLineSync();

    var contatoEncontrado =
        contatos.firstWhere((contato) => contato.nome == nome, orElse: () => Contato(nome: "", telefone: ""));
    
    if (contatoEncontrado.nome.isEmpty) {
      print("Contato não encontrado!");
    } else {
      contatos.remove(contatoEncontrado);
      print("Contato removido com sucesso!");
    }
  }

  void exibirContatos() {
    if (contatos.isEmpty) {
      print("Lista de contatos vazia!");
    } else {
      print("Lista de contatos:");
      contatos.forEach((contato) {
        print("Nome: ${contato.nome}");
        print("Telefone: ${contato.telefone}");
        print("==================");
      });
    }
  }
}

void main() {
  var listaContatos = ListaContatos();
  var opcao = "";

  while (opcao != "4") {
    print("Escolha uma opção:");
    print("1 - Adicionar contato");
    print("2 - Remover contato");
    print("3 - Exibir contatos");
    print("4 - Sair");
    stdout.write("Opção: ");
    opcao = stdin.readLineSync()!;

    switch (opcao) {
      case "1":
        listaContatos.adicionarContato();
        break;
      case "2":
        listaContatos.removerContato();
        break;
      case "3":
        listaContatos.exibirContatos();
        break;
      case "4":
        print("Saindo...");
        break;
      default:
        print("Opção inválida!");
    }
    print("==================");
  }
}
```

Neste código, temos duas classes principais: `Contato` e `ListaContatos`. A classe `Contato` representa um contato com nome e telefone. A classe `ListaContatos` possui uma lista de contatos e métodos para adicionar, remover e exibir os contatos.

No método `main`, é criada uma instância da classe `ListaContatos` e é apresentado um menu ao usuário com as opções de adicionar, remover, exibir contatos ou sair do programa. Dependendo da opção escolhida, os métodos correspondentes da classe `ListaContatos` são chamados.

O código utiliza entrada e saída de dados pelo terminal, permitindo que o usuário interaja com o programa.