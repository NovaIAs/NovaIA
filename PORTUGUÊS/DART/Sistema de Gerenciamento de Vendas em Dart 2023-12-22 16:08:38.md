Claro! Aqui está um código complexo em Dart que envolve um sistema de gerenciamento de vendas de uma loja fictícia. Vou explicar cada parte do código em detalhes.

```dart
import 'dart:io';

void main() {
  Loja loja = Loja();

  while (true) {
    print("=== SISTEMA DE GERENCIAMENTO DE VENDAS ===");
    print("1 - Cadastrar produto");
    print("2 - Realizar venda");
    print("3 - Verificar estoque");
    print("4 - Verificar vendas");
    print("5 - Sair");

    stdout.write("Opção: ");
    int opcao = int.parse(stdin.readLineSync());

    switch (opcao) {
      case 1:
        cadastrarProduto(loja);
        break;
      case 2:
        realizarVenda(loja);
        break;
      case 3:
        verificarEstoque(loja);
        break;
      case 4:
        verificarVendas(loja);
        break;
      case 5:
        print("Encerrando o sistema...");
        return;
      default:
        print("Opção inválida. Por favor, tente novamente.");
        break;
    }

    print("\n");
  }
}

class Loja {
  List<Produto> estoque = [];
  List<Venda> vendas = [];
}

class Produto {
  String nome;
  double preco;
  int quantidade;

  Produto(this.nome, this.preco, this.quantidade);
}

class Venda {
  List<Produto> produtos;
  double valorTotal;

  Venda(this.produtos, this.valorTotal);
}

void cadastrarProduto(Loja loja) {
  stdout.write("Nome do produto: ");
  String nome = stdin.readLineSync();

  stdout.write("Preço do produto: ");
  double preco = double.parse(stdin.readLineSync());

  stdout.write("Quantidade disponível: ");
  int quantidade = int.parse(stdin.readLineSync());

  Produto produto = Produto(nome, preco, quantidade);
  loja.estoque.add(produto);

  print("Produto cadastrado com sucesso!");
}

void realizarVenda(Loja loja) {
  if (loja.estoque.isEmpty) {
    print("Não há produtos disponíveis no estoque!");
    return;
  }

  List<Produto> produtosVenda = [];

  while (true) {
    print("=== ADICIONAR PRODUTO À VENDA ===");
    print("Produtos disponíveis:");

    for (int i = 0; i < loja.estoque.length; i++) {
      print("${i + 1} - ${loja.estoque[i].nome} (R\$ ${loja.estoque[i].preco})");
    }

    print("0 - Concluir venda");

    stdout.write("Opção: ");
    int opcao = int.parse(stdin.readLineSync());

    if (opcao == 0) {
      break;
    } else if (opcao < 1 || opcao > loja.estoque.length) {
      print("Opção inválida. Por favor, tente novamente.");
      continue;
    }

    Produto produtoSelecionado = loja.estoque[opcao - 1];

    stdout.write("Quantidade desejada: ");
    int quantidadeDesejada = int.parse(stdin.readLineSync());

    if (quantidadeDesejada > produtoSelecionado.quantidade) {
      print("Quantidade insuficiente no estoque!");
      continue;
    }

    Produto produtoVenda = Produto(produtoSelecionado.nome, produtoSelecionado.preco, quantidadeDesejada);
    produtosVenda.add(produtoVenda);
    produtoSelecionado.quantidade -= quantidadeDesejada;

    print("Produto adicionado à venda!");
  }

  double valorTotal = 0;

  for (var produto in produtosVenda) {
    valorTotal += produto.preco * produto.quantidade;
  }

  Venda venda = Venda(produtosVenda, valorTotal);
  loja.vendas.add(venda);

  print("Venda realizada com sucesso!");
}

void verificarEstoque(Loja loja) {
  if (loja.estoque.isEmpty) {
    print("Não há produtos disponíveis no estoque!");
    return;
  }

  print("=== ESTOQUE ===");
  for (int i = 0; i < loja.estoque.length; i++) {
    print("${i + 1} - ${loja.estoque[i].nome} (R\$ ${loja.estoque[i].preco}) - ${loja.estoque[i].quantidade} unidades");
  }
}

void verificarVendas(Loja loja) {
  if (loja.vendas.isEmpty) {
    print("Não há vendas realizadas!");
    return;
  }

  print("=== VENDAS ===");
  for (int i = 0; i < loja.vendas.length; i++) {
    print("Venda ${i + 1}:");
    for (var produto in loja.vendas[i].produtos) {
      print("${produto.nome} (R\$ ${produto.preco}) - ${produto.quantidade} unidades");
    }
    print("Valor total: R\$ ${loja.vendas[i].valorTotal}");
    print("\n");
  }
}
```

Neste código, criamos uma classe chamada `Loja`, que possui duas listas: `estoque`, que armazena os produtos disponíveis, e `vendas`, que armazena as vendas realizadas. 

Também temos a classe `Produto`, que representa cada item do estoque, com atributos como o nome, preço e quantidade disponível. E a classe `Venda`, que representa cada venda realizada, com a lista de produtos vendidos e o valor total da venda.

No método `main()`, temos um loop que exibe um menu para o usuário escolher entre cadastrar um produto, realizar uma venda, verificar o estoque, verificar as vendas ou sair do sistema.

Cada opção do menu está associada a uma função específica, como `cadastrarProduto()`, `realizarVenda()`, `verificarEstoque()` e `verificarVendas()`. Essas funções interagem com o usuário para obter os dados necessários, como nome, preço e quantidade do produto, ou a seleção dos produtos em uma venda. Em seguida, atualizam as listas `estoque` e `vendas` da classe `Loja`, de acordo com as ações realizadas.

O código também verifica se as listas estão vazias antes de exibir o estoque ou as vendas, para evitar exibir mensagens de erro quando não há dados disponíveis.

Espero que este código complexo em Dart atenda às suas expectativas!