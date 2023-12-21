Claro! Aqui está um código complexo em Kotlin que implementa um sistema de gerenciamento de biblioteca:

```kotlin
import java.util.*

class Livro(val titulo: String, val autor: String, val anoPublicacao: Int) {
    override fun toString(): String {
        return "Título: $titulo, Autor: $autor, Ano de Publicação: $anoPublicacao"
    }
}

class Biblioteca {
    private val livros: MutableList<Livro> = mutableListOf()

    fun adicionarLivro(livro: Livro) {
        livros.add(livro)
        println("Livro adicionado com sucesso!")
    }

    fun removerLivro(titulo: String) {
        val livro = livros.find { it.titulo == titulo }
        if (livro != null) {
            livros.remove(livro)
            println("Livro removido com sucesso!")
        } else {
            println("Livro não encontrado na biblioteca.")
        }
    }

    fun listarLivros() {
        if (livros.isEmpty()) {
            println("A biblioteca está vazia.")
        } else {
            println("Livros na biblioteca:")
            livros.forEach { println(it) }
        }
    }
}

fun main() {
    val biblioteca = Biblioteca()
    val scanner = Scanner(System.`in`)

    loop@ while (true) {
        println("========== BIBLIOTECA ==========")
        println("1. Adicionar livro")
        println("2. Remover livro")
        println("3. Listar livros")
        println("0. Sair")
        println("================================")

        print("Digite a opção desejada: ")
        when (scanner.nextInt()) {
            1 -> {
                print("Digite o título do livro: ")
                val titulo = scanner.next()
                print("Digite o nome do autor: ")
                val autor = scanner.next()
                print("Digite o ano de publicação: ")
                val anoPublicacao = scanner.nextInt()

                val livro = Livro(titulo, autor, anoPublicacao)
                biblioteca.adicionarLivro(livro)
            }
            2 -> {
                print("Digite o título do livro a ser removido: ")
                val titulo = scanner.next()
                biblioteca.removerLivro(titulo)
            }
            3 -> {
                biblioteca.listarLivros()
            }
            0 -> {
                break@loop
            }
            else -> {
                println("Opção inválida. Tente novamente.")
            }
        }
    }

    scanner.close()
}
```

Neste código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` possui três propriedades (título, autor e ano de publicação) e uma função `toString()` que retorna uma string formatada com os dados do livro.

A classe `Biblioteca` possui uma lista mutável de `Livros` e três funções principais: `adicionarLivro`, `removerLivro` e `listarLivros`. A função `adicionarLivro` recebe um objeto `Livro` e o adiciona à lista de livros da biblioteca. A função `removerLivro` recebe o título do livro a ser removido e o remove da lista, se encontrado. A função `listarLivros` lista todos os livros da biblioteca na saída.

No `main`, temos um loop que exibe um menu com opções para o usuário interagir com a biblioteca. O usuário pode adicionar um livro, remover um livro, listar todos os livros ou sair do programa. Cada opção chama as respectivas funções da classe `Biblioteca` para executar a ação desejada.

O código utiliza a classe `Scanner` para receber entrada de dados do usuário. A opção "0" no menu encerra o loop e finaliza o programa.

Esse código complexo em Kotlin implementa um sistema de gerenciamento de biblioteca que permite ao usuário adicionar, remover e listar livros. Ele utiliza classes, listas, iterações e estruturas de controle para fornecer a funcionalidade desejada.