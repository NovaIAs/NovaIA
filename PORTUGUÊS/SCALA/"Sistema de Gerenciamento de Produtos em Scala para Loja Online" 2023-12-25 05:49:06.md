Claro! Aqui está um código complexo em Scala que cria um sistema de gerenciamento de produtos em uma loja online:

```scala
import scala.collection.mutable.ArrayBuffer

// Classe Produto representa um produto na loja
case class Produto(id: Int, nome: String, preco: Double)

// Classe Carrinho representa o carrinho de compras de um cliente
class Carrinho {
  private val produtos: ArrayBuffer[Produto] = ArrayBuffer.empty[Produto]

  // Adiciona um produto ao carrinho
  def adicionarProduto(produto: Produto): Unit = {
    produtos += produto
  }

  // Remove um produto do carrinho
  def removerProduto(produto: Produto): Unit = {
    produtos -= produto
  }

  // Calcula o valor total dos produtos no carrinho
  def calcularTotal(): Double = {
    produtos.map(_.preco).sum
  }

  // Limpa o carrinho, removendo todos os produtos
  def limparCarrinho(): Unit = {
    produtos.clear()
  }
}

object LojaOnline {
  def main(args: Array[String]): Unit = {
    val carrinho = new Carrinho()

    // Cria alguns produtos
    val produto1 = Produto(1, "Camiseta", 29.99)
    val produto2 = Produto(2, "Calça", 59.99)
    val produto3 = Produto(3, "Tênis", 99.99)

    // Adiciona os produtos ao carrinho
    carrinho.adicionarProduto(produto1)
    carrinho.adicionarProduto(produto2)
    carrinho.adicionarProduto(produto3)

    // Calcula o total e exibe na tela
    val total = carrinho.calcularTotal()
    println(s"Total: R$$total")

    // Remove um produto e recalcula o total
    carrinho.removerProduto(produto2)
    val novoTotal = carrinho.calcularTotal()
    println(s"Novo Total: R$$novoTotal")

    // Limpa o carrinho
    carrinho.limparCarrinho()
    val totalFinal = carrinho.calcularTotal()
    println(s"Total Final: R$$totalFinal")
  }
}
```

Explicação do código:

1. Começamos importando `scala.collection.mutable.ArrayBuffer`, que é uma classe do Scala que nos permite criar uma coleção mutável de objetos.
2. Em seguida, definimos a classe `Produto`, que representa um produto na loja. Ela possui três atributos: `id`, `nome` e `preco`.
3. Depois, definimos a classe `Carrinho`, que representa o carrinho de compras de um cliente. Ela contém um ArrayBuffer de produtos, onde os produtos adicionados serão armazenados.
4. A classe `Carrinho` possui quatro métodos principais:
   - `adicionarProduto`: adiciona um produto ao carrinho, passando o produto como parâmetro.
   - `removerProduto`: remove um produto do carrinho, passando o produto como parâmetro.
   - `calcularTotal`: calcula o valor total dos produtos no carrinho, somando os preços de cada produto.
   - `limparCarrinho`: remove todos os produtos do carrinho, limpando-o completamente.
5. Em seguida, criamos o objeto `LojaOnline` que contém o método `main`, que é o ponto de entrada do programa. Aqui, criamos um novo carrinho, alguns produtos e realizamos algumas operações:
   - Adicionamos os produtos ao carrinho usando o método `adicionarProduto`.
   - Calculamos o total usando o método `calcularTotal` e exibimos na tela.
   - Removemos um produto do carrinho usando o método `removerProduto`.
   - Calculamos o novo total e exibimos na tela.
   - Limpamos o carrinho usando o método `limparCarrinho`.
   - Calculamos o total final e exibimos na tela.

Esse código simula um sistema básico de gerenciamento de produtos em uma loja online, permitindo adicionar, remover, calcular o total e limpar o carrinho de compras.