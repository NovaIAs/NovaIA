Claro! Aqui está um exemplo de código em Kotlin. Este código é um sistema de gerenciamento de biblioteca, onde é possível cadastrar livros, pesquisar por livro, emprestar e devolver livros, gerar relatórios, entre outras funcionalidades. O código está dividido em diferentes classes para melhor organização e reutilização de código.

```kotlin
import java.time.LocalDate

class Livro(val id: Int, val titulo: String, val autor: String, val anoPublicacao: Int) {
    var emprestado: Boolean = false
    var dataEmprestimo: LocalDate? = null
    var dataDevolucao: LocalDate? = null

    fun emprestar() {
        if (!emprestado) {
            emprestado = true
            dataEmprestimo = LocalDate.now()
            dataDevolucao = dataEmprestimo?.plusDays(15)
            println("Livro $titulo emprestado com sucesso.")
        } else {
            println("Este livro já está emprestado.")
        }
    }

    fun devolver() {
        if (emprestado) {
            emprestado = false
            println("Livro $titulo devolvido com sucesso.")
        } else {
            println("Este livro não está emprestado.")
        }
    }

    override fun toString(): String {
        return "Livro(id=$id, titulo='$titulo', autor='$autor', anoPublicacao=$anoPublicacao, emprestado=$emprestado, dataEmprestimo=$dataEmprestimo, dataDevolucao=$dataDevolucao)"
    }
}

class Biblioteca {
    private val livros: MutableList<Livro> = mutableListOf()

    fun cadastrarLivro(livro: Livro) {
        livros.add(livro)
        println("Livro cadastrado com sucesso.")
    }

    fun pesquisarLivroPorTitulo(titulo: String): List<Livro> {
        val livrosEncontrados = livros.filter { it.titulo.equals(titulo, ignoreCase = true) }
        if (livrosEncontrados.isNotEmpty()) {
            println("Livros encontrados:")
            livrosEncontrados.forEach { println(it) }
        } else {
            println("Nenhum livro encontrado com o título '$titulo'.")
        }
        return livrosEncontrados
    }

    fun pesquisarLivroPorAutor(autor: String): List<Livro> {
        val livrosEncontrados = livros.filter { it.autor.equals(autor, ignoreCase = true) }
        if (livrosEncontrados.isNotEmpty()) {
            println("Livros encontrados:")
            livrosEncontrados.forEach { println(it) }
        } else {
            println("Nenhum livro encontrado do autor '$autor'.")
        }
        return livrosEncontrados
    }

    fun gerarRelatorioLivrosEmprestados(): List<Livro> {
        val livrosEmprestados = livros.filter { it.emprestado }
        if (livrosEmprestados.isNotEmpty()) {
            println("Livros emprestados:")
            livrosEmprestados.forEach { println(it) }
        } else {
            println("Nenhum livro emprestado.")
        }
        return livrosEmprestados
    }
}

fun main() {
    val biblioteca = Biblioteca()

    val livro1 = Livro(1, "Dom Casmurro", "Machado de Assis", 1899)
    val livro2 = Livro(2, "O Pequeno Príncipe", "Antoine de Saint-Exupéry", 1943)
    val livro3 = Livro(3, "1984", "George Orwell", 1949)
    val livro4 = Livro(4, "O Senhor dos Anéis", "J.R.R. Tolkien", 1954)

    biblioteca.cadastrarLivro(livro1)
    biblioteca.cadastrarLivro(livro2)
    biblioteca.cadastrarLivro(livro3)
    biblioteca.cadastrarLivro(livro4)

    livro1.emprestar()
    livro2.emprestar()

    biblioteca.pesquisarLivroPorTitulo("dom casmurro")
    biblioteca.pesquisarLivroPorAutor("machado de assis")

    livro1.devolver()

    biblioteca.gerarRelatorioLivrosEmprestados()
}
```

Neste código, temos a classe `Livro` que representa um livro da biblioteca. Cada livro possui um id, título, autor e ano de publicação. Também possui os atributos `emprestado`, `dataEmprestimo` e `dataDevolucao` para controle de empréstimos.

A classe `Biblioteca` é responsável por gerenciar os livros. Ela possui uma lista de livros e métodos para cadastrar, pesquisar por título ou autor e gerar relatório de livros emprestados.

Na função `main`, são criados alguns livros e realizadas ações como cadastrar livros, emprestar, devolver e pesquisar por livros.

Espero que esse código atenda às suas expectativas e seja útil para você!