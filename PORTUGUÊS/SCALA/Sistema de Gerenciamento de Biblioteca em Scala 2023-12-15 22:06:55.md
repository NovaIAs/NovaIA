Claro! Vou criar um código complexo em Scala para você. Este código é um exemplo de um sistema de gerenciamento de biblioteca, que permite adicionar, remover e pesquisar livros. Vamos dividir o código em diferentes classes para facilitar a organização e a manutenção.

```scala
import scala.collection.mutable.ListBuffer

// Classe para representar um livro
case class Livro(titulo: String, autor: String, anoPublicacao: Int)

// Classe para representar a biblioteca
class Biblioteca {
  private val livros: ListBuffer[Livro] = ListBuffer()

  // Método para adicionar um livro à biblioteca
  def adicionarLivro(livro: Livro): Unit = {
    livros += livro
    println(s"Livro '${livro.titulo}' adicionado à biblioteca.")
  }

  // Método para remover um livro da biblioteca
  def removerLivro(titulo: String): Unit = {
    val livroRemovido = livros.find(_.titulo == titulo)
    livroRemovido match {
      case Some(livro) =>
        livros -= livro
        println(s"Livro '${livro.titulo}' removido da biblioteca.")
      case None =>
        println(s"Livro com título '$titulo' não encontrado na biblioteca.")
    }
  }

  // Método para pesquisar livros por título ou autor
  def pesquisarLivros(termo: String): Seq[Livro] = {
    val resultado = livros.filter(livro =>
      livro.titulo.toLowerCase.contains(termo.toLowerCase) ||
      livro.autor.toLowerCase.contains(termo.toLowerCase)
    )
    resultado.foreach(livro =>
      println(s"Livro encontrado: '${livro.titulo}' por ${livro.autor} (${livro.anoPublicacao})")
    )
    resultado
  }
}

// Classe principal que contém o ponto de entrada do programa
object Principal {
  def main(args: Array[String]): Unit = {
    val biblioteca = new Biblioteca()

    val livro1 = Livro("Dom Quixote", "Miguel de Cervantes", 1605)
    val livro2 = Livro("Cem Anos de Solidão", "Gabriel Garcia Marquez", 1967)
    val livro3 = Livro("1984", "George Orwell", 1949)

    biblioteca.adicionarLivro(livro1)
    biblioteca.adicionarLivro(livro2)
    biblioteca.adicionarLivro(livro3)

    biblioteca.removerLivro("Dom Quixote")
    biblioteca.removerLivro("Crime e Castigo")

    biblioteca.pesquisarLivros("solidão")
  }
}
```

Neste código, temos a classe `Livro`, que representa um livro com os atributos `titulo`, `autor` e `anoPublicacao`. Em seguida, temos a classe `Biblioteca`, que possui uma lista mutável de livros e os métodos `adicionarLivro`, `removerLivro` e `pesquisarLivros`.

Na classe principal `Principal`, criamos uma instância da `Biblioteca` e adicionamos alguns livros utilizando o método `adicionarLivro`. Em seguida, removemos um livro utilizando o método `removerLivro`. Por fim, realizamos uma pesquisa de livros pelo termo "solidão" utilizando o método `pesquisarLivros`.

Espero que este código complexo atenda às suas expectativas!