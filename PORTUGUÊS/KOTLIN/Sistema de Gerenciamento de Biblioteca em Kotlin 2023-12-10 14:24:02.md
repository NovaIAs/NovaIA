Claro! Aqui está um código em Kotlin que implementa um sistema de gerenciamento de biblioteca. Ele permite adicionar e remover livros, exibir a lista de livros disponíveis e pesquisar por um livro específico. Vamos lá:

```kotlin
import java.util.*

data class Livro(val titulo: String, val autor: String)

class Biblioteca {
    private val livros: MutableList<Livro> = mutableListOf()

    fun adicionarLivro(titulo: String, autor: String) {
        val livro = Livro(titulo, autor)
        livros.add(livro)
        println("Livro adicionado: $titulo")
    }

    fun removerLivro(titulo: String) {
        val livroEncontrado = livros.find { it.titulo == titulo }
        if (livroEncontrado != null) {
            livros.remove(livroEncontrado)
            println("Livro removido: $titulo")
        } else {
            println("Livro não encontrado: $titulo")
        }
    }

    fun exibirLivros() {
        if (livros.isEmpty()) {
            println("Não há livros disponíveis.")
        } else {
            println("Livros disponíveis:")
            livros.forEach {
                println("${it.titulo} - ${it.autor}")
            }
        }
    }

    fun pesquisarLivro(titulo: String) {
        val livroEncontrado = livros.find { it.titulo == titulo }
        if (livroEncontrado != null) {
            println("Livro encontrado: ${livroEncontrado.titulo} - ${livroEncontrado.autor}")
        } else {
            println("Livro não encontrado: $titulo")
        }
    }
}

fun main() {
    val biblioteca = Biblioteca()

    // Adicionar livros
    biblioteca.adicionarLivro("Dom Quixote", "Miguel de Cervantes")
    biblioteca.adicionarLivro("Grande Sertão: Veredas", "João Guimarães Rosa")
    biblioteca.adicionarLivro("1984", "George Orwell")

    // Exibir livros
    biblioteca.exibirLivros()

    // Pesquisar livro
    biblioteca.pesquisarLivro("Grande Sertão: Veredas")

    // Remover livro
    biblioteca.removerLivro("1984")

    // Exibir livros atualizados
    biblioteca.exibirLivros()
}
```

Neste código, definimos a classe `Livro` que possui duas propriedades: `titulo` e `autor`. Em seguida, temos a classe `Biblioteca`, que gerencia uma lista de livros usando a estrutura de dados `MutableList`. Essa classe possui métodos para adicionar, remover, exibir e pesquisar livros.

Na função `main`, criamos uma instância da classe `Biblioteca` e realizamos algumas operações, como adicionar livros, exibir a lista atualizada, pesquisar um livro específico e remover um livro. No final, exibimos novamente a lista de livros para verificar as alterações.

Esse código é apenas um exemplo de implementação de um sistema de gerenciamento de biblioteca em Kotlin. Ele pode ser expandido e personalizado de acordo com as necessidades específicas do projeto.