import scala.io.Source
import scala.collection.mutable.ArrayBuffer

// Classe para representar um livro
case class Livro(titulo: String, autor: String, anoPublicacao: Int)

// Função para ordenar a lista de livros por ordem de publicação
def ordenarPorAnoPublicacao(livros: List[Livro]): List[Livro] = {
  livros.sortBy(_.anoPublicacao)
}

// Função para filtrar livros de um determinado autor
def filtrarPorAutor(livros: List[Livro], autor: String): List[Livro] = {
  livros.filter(_.autor == autor)
}

// Função para ler um arquivo CSV contendo informações sobre livros
def lerLivros(caminhoArquivo: String): List[Livro] = {
  val linhas = Source.fromFile(caminhoArquivo).getLines().toList
  val livros = ArrayBuffer[Livro]()

  for (linha <- linhas) {
    val campos = linha.split(";")
    if (campos.length == 3) {
      val titulo = campos(0)
      val autor = campos(1)
      val anoPublicacao = campos(2).toInt
      livros += Livro(titulo, autor, anoPublicacao)
    }
  }

  livros.toList
}

// Código principal
def main(): Unit = {
  val caminhoArquivo = "livros.csv"
  
  // Lê os livros do arquivo CSV
  val livros = lerLivros(caminhoArquivo)
  println("Livros lidos:")
  livros.foreach(livro => println(s"${livro.titulo} - ${livro.autor} (${livro.anoPublicacao})"))
  
  // Ordena os livros por ordem de publicação
  val livrosOrdenados = ordenarPorAnoPublicacao(livros)
  println("\nLivros ordenados por ano de publicação:")
  livrosOrdenados.foreach(livro => println(s"${livro.titulo} - ${livro.autor} (${livro.anoPublicacao})"))
  
  // Filtra os livros de um determinado autor
  val autor = "Machado de Assis"
  val livrosFiltrados = filtrarPorAutor(livrosOrdenados, autor)
  println(s"\nLivros do autor $autor:")
  livrosFiltrados.foreach(livro => println(s"${livro.titulo} - ${livro.autor} (${livro.anoPublicacao})"))
}

// Chamada do código principal
main()

Neste código em Scala, implementei algumas funcionalidades relacionadas a um sistema de gerenciamento de livros. 

A classe "Livro" representa um livro e tem três atributos: o "titulo" do livro, o "autor" e o "anoPublicacao" do livro.

A função "ordenarPorAnoPublicacao" recebe uma lista de livros e retorna uma nova lista com os livros ordenados por ordem de publicação, do mais antigo para o mais recente.

A função "filtrarPorAutor" recebe uma lista de livros e um autor como parâmetros, e retorna uma nova lista contendo apenas os livros do autor informado.

A função "lerLivros" recebe o caminho de um arquivo CSV contendo informações sobre livros, lê as linhas do arquivo e cria instâncias da classe "Livro" a partir dos dados. Em seguida, retorna uma lista de livros.

No código principal, a função "main" realiza as seguintes operações:
1. Lê os livros de um arquivo CSV chamado "livros.csv" usando a função "lerLivros" e os imprime na tela.
2. Ordena os livros por ordem de publicação usando a função "ordenarPorAnoPublicacao" e os imprime na tela.
3. Filtra os livros de um determinado autor usando a função "filtrarPorAutor" e os imprime na tela.

Essas são apenas algumas funcionalidades básicas implementadas neste código, mas você pode adicionar ou modificar conforme necessário.