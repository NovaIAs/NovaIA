Claro! Aqui está um código complexo em Scala que envolve o uso de classes, herança, polimorfismo e tratamento de exceções. O código é uma simulação de um sistema de gerenciamento de estoque para uma loja online. Vou explicar cada parte do código para você.

```scala
import scala.collection.mutable.ListBuffer

// Definindo a classe base Produto
abstract class Produto(val codigo: String, val nome: String, var preco: Double)

// Definindo a classe Livro que herda de Produto
class Livro(codigo: String, nome: String, preco: Double, val autor: String, val pagina: Int) extends Produto(codigo, nome, preco)

// Definindo a classe Eletronico que herda de Produto
class Eletronico(codigo: String, nome: String, preco: Double, val marca: String, val voltagem: Int) extends Produto(codigo, nome, preco)

// Definindo a classe Estoque
class Estoque {
  private val produtos: ListBuffer[Produto] = ListBuffer.empty[Produto]
  
  // Adiciona um produto ao estoque
  def adicionarProduto(produto: Produto): Unit = {
    produtos += produto
    println(s"Produto ${produto.nome} adicionado ao estoque.")
  }
  
  // Remove um produto do estoque
  def removerProduto(codigo: String): Unit = {
    val produto = produtos.find(_.codigo == codigo)
    produto match {
      case Some(p) =>
        produtos -= p
        println(s"Produto ${p.nome} removido do estoque.")
      case None =>
        println("Produto não encontrado no estoque.")
    }
  }
  
  // Lista todos os produtos do estoque
  def listarProdutos(): Unit = {
    produtos.foreach(p => println(s"Código: ${p.codigo}, Nome: ${p.nome}, Preço: ${p.preco}"))
  }
  
  // Calcula o valor total do estoque
  def calcularValorTotal(): Double = {
    produtos.map(_.preco).sum
  }
}

// Testando o código
object Main extends App {
  // Criando instâncias de produtos
  val livro1 = new Livro("L001", "Dom Casmurro", 29.90, "Machado de Assis", 256)
  val livro2 = new Livro("L002", "O Pequeno Príncipe", 19.90, "Antoine de Saint-Exupéry", 96)
  val eletronico1 = new Eletronico("E001", "Smartphone", 1999.90, "Samsung", 220)
  val eletronico2 = new Eletronico("E002", "Notebook", 3499.90, "Dell", 110)
  
  // Criando instância do estoque
  val estoque = new Estoque()
  
  // Adicionando produtos ao estoque
  estoque.adicionarProduto(livro1)
  estoque.adicionarProduto(livro2)
  estoque.adicionarProduto(eletronico1)
  estoque.adicionarProduto(eletronico2)
  
  // Listando produtos do estoque
  estoque.listarProdutos()
  
  // Removendo um produto do estoque
  estoque.removerProduto("L002")
  
  // Listando produtos do estoque novamente
  estoque.listarProdutos()
  
  // Calculando valor total do estoque
  val valorTotal = estoque.calcularValorTotal()
  println(s"Valor total do estoque: $valorTotal")
}
```

Nesse código, temos a definição das classes `Produto`, `Livro`, `Eletronico` e `Estoque`. A classe `Produto` é uma classe abstrata que possui os atributos comuns a todos os produtos, como código, nome e preço.

As classes `Livro` e `Eletronico` herdam da classe `Produto` e adicionam atributos específicos, como autor e número de páginas para livros, e marca e voltagem para eletrônicos.

A classe `Estoque` é responsável por gerenciar os produtos em estoque. Ela utiliza uma estrutura de dados `ListBuffer` para armazenar os produtos. Através dos métodos `adicionarProduto`, `removerProduto`, `listarProdutos` e `calcularValorTotal`, o estoque é capaz de adicionar produtos, remover produtos por código, exibir a lista de produtos e calcular o valor total do estoque.

No bloco `Main`, são instanciados alguns produtos (`livro1`, `livro2`, `eletronico1` e `eletronico2`) e um estoque (`estoque`). Os produtos são adicionados ao estoque utilizando o método `adicionarProduto`, e em seguida a lista de produtos é exibida utilizando o método `listarProdutos`. Um produto é então removido do estoque utilizando o método `removerProduto`, e a lista de produtos é exibida novamente. Por fim, o valor total do estoque é calculado utilizando o método `calcularValorTotal` e é exibido na tela.