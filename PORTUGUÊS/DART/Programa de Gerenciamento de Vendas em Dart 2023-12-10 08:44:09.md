Claro! Abaixo está um exemplo de um código complexo em Dart que consiste em um programa de gerenciamento de vendas para uma loja fictícia.

```dart
import 'dart:io';

class Produto {
  int codigo;
  String nome;
  double preco;
  int quantidade;

  Produto(this.codigo, this.nome, this.preco, this.quantidade);

  double calcularTotal() {
    return preco * quantidade;
  }

  void imprimirProduto() {
    print("Código: $codigo");
    print("Nome: $nome");
    print("Preço: R\$ $preco");
    print("Quantidade: $quantidade");
    print("Total: R\$ ${calcularTotal()}");
  }
}

class Venda {
  List<Produto> itens;

  Venda() {
    itens = [];
  }

  void adicionarItem() {
    stdout.write("Digite o código do produto: ");
    int codigo = int.parse(stdin.readLineSync());

    stdout.write("Digite o nome do produto: ");
    String nome = stdin.readLineSync();

    stdout.write("Digite o preço do produto: ");
    double preco = double.parse(stdin.readLineSync());

    stdout.write("Digite a quantidade do produto: ");
    int quantidade = int.parse(stdin.readLineSync());

    Produto produto = Produto(codigo, nome, preco, quantidade);
    itens.add(produto);

    print("Produto adicionado com sucesso!");
  }

  void imprimirVenda() {
    if (itens.isEmpty) {
      print("Não há itens na venda.");
    } else {
      for (var produto in itens) {
        produto.imprimirProduto();
        print("-----------------------");
      }
    }
  }

  double calcularTotalVenda() {
    double total = 0;

    for (var produto in itens) {
      total += produto.calcularTotal();
    }

    return total;
  }
}

void main() {
  Venda venda = Venda();

  int opcao;

  do {
    print("========= MENU =========");
    print("1 - Adicionar item");
    print("2 - Imprimir venda");
    print("3 - Calcular total da venda");
    print("0 - Sair");
    print("========================");
    stdout.write("Digite a opção desejada: ");
    opcao = int.parse(stdin.readLineSync());

    switch (opcao) {
      case 1:
        venda.adicionarItem();
        break;
      case 2:
        venda.imprimirVenda();
        break;
      case 3:
        double total = venda.calcularTotalVenda();
        print("Total da venda: R\$ $total");
        break;
      case 0:
        print("Saindo do programa...");
        break;
      default:
        print("Opção inválida! Digite novamente.");
    }

    print("\n");

  } while (opcao != 0);
}
```

Neste código, há duas classes principais: `Produto` e `Venda`. A classe `Produto` representa um item que pode ser vendido e possui atributos como código, nome, preço e quantidade. Ela também possui métodos para calcular o total do produto e imprimir suas informações.

A classe `Venda` é responsável por gerenciar a venda de produtos. Ela possui uma lista de itens (`itens`) que são adicionados através do método `adicionarItem()`. Além disso, possui métodos para imprimir a venda (`imprimirVenda`), calcular o total da venda (`calcularTotalVenda`) e um método `main` que funciona como o menu do programa.

No menu principal, o usuário pode selecionar as opções de adicionar um item, imprimir a venda, calcular o total da venda ou sair do programa. O código utiliza entrada e saída de dados pelo console para interagir com o usuário.

Espero que este exemplo de código complexo em Dart tenha atendido às suas expectativas! Lembre-se que este código é apenas um exemplo e pode ser adaptado e expandido de acordo com suas necessidades.