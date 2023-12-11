Aqui está um exemplo de um código complexo em Kotlin que envolve a implementação de um sistema de gerenciamento de biblioteca:

```kotlin
import java.time.LocalDate

data class Livro(
    val titulo: String,
    val autor: String,
    val genero: String,
    val anoPublicacao: Int,
    var disponivel: Boolean = true
)

class Biblioteca {
    private val livros: MutableList<Livro> = mutableListOf()

    fun adicionarLivro(livro: Livro) {
        livros.add(livro)
    }

    fun removerLivro(livro: Livro) {
        livros.remove(livro)
    }

    fun buscarLivroPorTitulo(titulo: String): Livro? {
        return livros.find { it.titulo == titulo }
    }

    fun buscarLivrosPorAutor(autor: String): List<Livro> {
        return livros.filter { it.autor == autor }
    }

    fun buscarLivrosPorGenero(genero: String): List<Livro> {
        return livros.filter { it.genero == genero }
    }

    fun listarLivrosDisponiveis(): List<Livro> {
        return livros.filter { it.disponivel }
    }

    fun listarLivrosEmprestados(): List<Livro> {
        return livros.filterNot { it.disponivel }
    }

    fun emprestarLivro(livro: Livro, dataEmprestimo: LocalDate) {
        if (livro.disponivel) {
            livro.disponivel = false
            println("O livro ${livro.titulo} foi emprestado em $dataEmprestimo")
        } else {
            println("O livro ${livro.titulo} não está disponível para empréstimo")
        }
    }

    fun devolverLivro(livro: Livro, dataDevolucao: LocalDate) {
        if (!livro.disponivel) {
            livro.disponivel = true
            println("O livro ${livro.titulo} foi devolvido em $dataDevolucao")
        } else {
            println("O livro ${livro.titulo} já está disponível na biblioteca")
        }
    }
}

fun main() {
    val biblioteca = Biblioteca()

    val livro1 = Livro("Dom Casmurro", "Machado de Assis", "Romance", 1899)
    val livro2 = Livro("1984", "George Orwell", "Ficção Científica", 1949)
    val livro3 = Livro("A Menina que Roubava Livros", "Markus Zusak", "Drama", 2005)

    biblioteca.adicionarLivro(livro1)
    biblioteca.adicionarLivro(livro2)
    biblioteca.adicionarLivro(livro3)

    println("Livros disponíveis na biblioteca:")
    biblioteca.listarLivrosDisponiveis().forEach { println("- ${it.titulo}") }

    println()

    println("Buscando livro por título:")
    val livroEncontrado = biblioteca.buscarLivroPorTitulo("1984")
    if (livroEncontrado != null) {
        println("- Título: ${livroEncontrado.titulo}")
        println("- Autor: ${livroEncontrado.autor}")
        println("- Gênero: ${livroEncontrado.genero}")
        println("- Ano de Publicação: ${livroEncontrado.anoPublicacao}")
    } else {
        println("Livro não encontrado")
    }

    println()

    println("Emprestando livro:")
    val hoje = LocalDate.now()
    biblioteca.emprestarLivro(livro1, hoje)
    biblioteca.emprestarLivro(livro2, hoje)

    println()

    println("Devolvendo livro:")
    val dataDevolucao = hoje.plusDays(7)
    biblioteca.devolverLivro(livro1, dataDevolucao)
    biblioteca.devolverLivro(livro2, dataDevolucao)
}
```

Neste código, temos a definição da classe `Livro`, com propriedades como título, autor, gênero e ano de publicação. A classe `Biblioteca` é responsável por gerenciar os livros, permitindo adicionar, remover e buscar livros por título, autor e gênero, além de listar os livros disponíveis e emprestados. Também é possível emprestar e devolver livros, atualizando o status de disponibilidade.

No `main()`, são criados alguns livros e adicionados à biblioteca. Em seguida, são exibidos os livros disponíveis na biblioteca, buscado um livro por título, emprestado e devolvido livros.