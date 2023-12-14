Certamente! Aqui está um código complexo em Scala, que utiliza classes, herança, polimorfismo e manipulação de listas. O código simula um sistema de gerenciamento de uma livraria, onde é possível adicionar, remover e listar livros.

```scala
import scala.collection.mutable.ListBuffer

// Classe base para representar um livro
abstract class Livro(val titulo: String, val autor: String, val anoPublicacao: Int) {
  def descricao: String
}

// Classe para representar um livro de ficção científica
class FiccaoCientifica(titulo: String, autor: String, anoPublicacao: Int, val tema: String) extends Livro(titulo, autor, anoPublicacao) {
  override def descricao: String = s"[$anoPublicacao] $autor - $titulo (Ficção Científica) - Tema: $tema"
}

// Classe para representar um livro de fantasia
class Fantasia(titulo: String, autor: String, anoPublicacao: Int, val mundo: String) extends Livro(titulo, autor, anoPublicacao) {
  override def descricao: String = s"[$anoPublicacao] $autor - $titulo (Fantasia) - Mundo: $mundo"
}

// Classe que representa a livraria
class Livraria {
  private val livros: ListBuffer[Livro] = ListBuffer()

  // Adiciona um livro à livraria
  def adicionarLivro(livro: Livro): Unit = {
    livros += livro
    println(s"O livro '${livro.titulo}' foi adicionado à livraria.")
  }

  // Remove um livro da livraria
  def removerLivro(livro: Livro): Unit = {
    if (livros.contains(livro)) {
      livros -= livro
      println(s"O livro '${livro.titulo}' foi removido da livraria.")
    } else {
      println(s"O livro '${livro.titulo}' não está na livraria.")
    }
  }

  // Lista todos os livros da livraria
  def listarLivros(): Unit = {
    println("Livros disponíveis na livraria:")
    for (livro <- livros) {
      println(livro.descricao)
    }
  }
}

// Exemplo de uso
object Main {
  def main(args: Array[String]): Unit = {
    val livraria = new Livraria()

    val livro1 = new FiccaoCientifica("Duna", "Frank Herbert", 1965, "Planeta desértico")
    val livro2 = new FiccaoCientifica("Fundação", "Isaac Asimov", 1951, "Império Galáctico")
    val livro3 = new Fantasia("O Senhor dos Anéis", "J.R.R. Tolkien", 1954, "Terra Média")

    livraria.adicionarLivro(livro1)
    livraria.adicionarLivro(livro2)
    livraria.adicionarLivro(livro3)

    livraria.listarLivros()

    livraria.removerLivro(livro2)

    livraria.listarLivros()
  }
}
```

Neste código, começamos definindo duas classes filhas da classe base `Livro`: `FiccaoCientifica` e `Fantasia`. Cada uma dessas classes adiciona propriedades específicas ao livro, como o tema ou o mundo em que a história se passa. Ambas as classes também sobrescrevem o método `descricao` para retornar uma descrição personalizada do livro.

Em seguida, temos a classe `Livraria`, que possui uma lista mutável de livros. Ela oferece métodos para adicionar, remover e listar os livros disponíveis na livraria.

Por fim, temos um exemplo de uso no objeto `Main`. Criamos uma instância da `Livraria` e alguns livros, adicionamos os livros à livraria, listamos os livros e removemos um dos livros. Ao executar o código, será exibida a saída com as informações dos livros.

Espero que este código complexo em Scala atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais ajuda, fique à vontade para perguntar.