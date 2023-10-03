Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que realiza uma simulação de um sistema de vendas de uma loja:

```cool
class Produto {
  string nome
  int preco
  int quantidade

  metódo init(nome: String, preco: Int, quantidade: Int) : Produto {
    self.nome <- nome
    self.preco <- preco
    self.quantidade <- quantidade
    return self
  }

  metódo exibirDetalhes() : String {
    string detalhes <- "Nome: " + nome + ", Preço: " + str(preco) + ", Quantidade: " + str(quantidade)
    return detalhes
  }
}

class CarrinhoDeCompras {
  List<Produto> itens
  int total

  metódo init() : CarrinhoDeCompras {
    self.itens <- List<Produto>()
    self.total <- 0
    return self
  }

  metódo adicionarItem(produto: Produto, quantidade: Int) : Void {
    if quantidade <= produto.quantidade {
      produto.quantidade <- produto.quantidade - quantidade
      self.itens.append(produto)
      self.total <- self.total + (produto.preco * quantidade)
      outputString("Produto adicionado ao carrinho!")
    } else {
      outputString("Quantidade insuficiente em estoque!")
    }
  }

  metódo exibirCarrinho() : Void {
    if self.itens.length() > 0 {
      outputString("Carrinho de Compras:")
      foreach item in self.itens do {
        outputString(item.exibirDetalhes())
      }
      outputString("Total: " + str(self.total))
    } else {
      outputString("Carrinho vazio!")
    }
  }
}

class Loja {
  List<Produto> estoque
  CarrinhoDeCompras carrinho

  metódo init() : Loja {
    self.estoque <- List<Produto>()
    self.carrinho <- CarrinhoDeCompras.init()
    return self
  }

  metódo adicionarProduto(nome: String, preco: Int, quantidade: Int) : Void {
    Produto produto <- Produto.init(nome, preco, quantidade)
    self.estoque.append(produto)
    outputString("Produto adicionado ao estoque!")
  }

  metódo realizarCompra() : Void {
    if self.estoque.length() > 0 {
      outputString("Produtos disponíveis:")
      foreach produto in self.estoque do {
        outputString(produto.exibirDetalhes())
      }
      string nomeProduto <- inputString("Digite o nome do produto que deseja comprar:")
      int quantidade <- inputInt("Digite a quantidade desejada:")
      Produto produtoSelecionado <- self.estoque.find(nomeProduto)
      if produtoSelecionado != void {
        self.carrinho.adicionarItem(produtoSelecionado, quantidade)
        self.estoque.remove(produtoSelecionado)
      } else {
        outputString("Produto não encontrado!")
      }
    } else {
      outputString("Estoque vazio!")
    }
  }

  metódo exibirRelatorioEstoque() : Void {
    if self.estoque.length() > 0 {
      outputString("Relatório de Estoque:")
      foreach produto in self.estoque do {
        outputString(produto.exibirDetalhes())
      }
    } else {
      outputString("Estoque vazio!")
    }
  }
}

metódo principal() : Int {
  Loja minhaLoja <- Loja.init()
  minhaLoja.adicionarProduto("Camiseta", 50, 10)
  minhaLoja.adicionarProduto("Calça", 100, 5)
  minhaLoja.adicionarProduto("Tênis", 200, 3)
  minhaLoja.realizarCompra()
  minhaLoja.exibirRelatorioEstoque()
  minhaLoja.carrinho.exibirCarrinho()
  return 0
}
```

Neste código, temos três classes principais: "Produto", "CarrinhoDeCompras" e "Loja". A classe "Produto" representa um produto da loja, com atributos como nome, preço e quantidade em estoque. O método "exibirDetalhes" retorna uma string com as informações do produto.

A classe "CarrinhoDeCompras" representa o carrinho de compras do cliente, com uma lista de produtos e o total a ser pago. O método "adicionarItem" permite adicionar um produto ao carrinho, desde que haja quantidade suficiente em estoque. O método "exibirCarrinho" mostra os produtos presentes no carrinho e o total a ser pago.

A classe "Loja" é responsável por gerenciar o estoque e as compras. O método "adicionarProduto" permite adicionar um novo produto ao estoque da loja. O método "realizarCompra" permite ao cliente selecionar um produto e a quantidade desejada, adicionando-o ao carrinho e subtraindo a quantidade do estoque. O método "exibirRelatorioEstoque" mostra todos os produtos disponíveis em estoque.

No método "principal", é criada uma instância da classe "Loja" e são adicionados alguns produtos ao estoque. Em seguida, é realizada uma compra, exibido um relatório do estoque e, por fim, exibido o carrinho de compras.

Espero que este código atenda às suas expectativas!