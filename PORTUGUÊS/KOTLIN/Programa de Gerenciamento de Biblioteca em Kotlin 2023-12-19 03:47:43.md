Claro! Aqui está um exemplo de um código complexo em Kotlin. Este código é um programa de gerenciamento de biblioteca, onde os usuários podem adicionar, visualizar e remover livros da biblioteca. Ele utiliza classes, herança, listas, loops, condicionais e funções para realizar as operações necessárias. Vou explicar cada parte do código para você.

```kotlin
import java.util.Scanner

// Classe base para representar um livro
open class Livro(val titulo: String, val autor: String, val anoPublicacao: Int) {
    override fun toString(): String {
        return "Título: $titulo, Autor: $autor, Ano de Publicação: $anoPublicacao"
    }
}

// Classe derivada que adiciona um campo para o número de páginas do livro
class LivroComPaginas(titulo: String, autor: String, anoPublicacao: Int, val numPaginas: Int) : Livro(titulo, autor, anoPublicacao) {
    override fun toString(): String {
        return super.toString() + ", Número de Páginas: $numPaginas"
    }
}

// Classe principal que representa a biblioteca e suas operações
class Biblioteca {
    val livros = mutableListOf<Livro>() // Lista de livros na biblioteca

    // Função para adicionar um livro na biblioteca
    fun adicionarLivro(livro: Livro) {
        livros.add(livro)
        println("Livro adicionado com sucesso!")
    }

    // Função para visualizar todos os livros da biblioteca
    fun visualizarLivros() {
        if (livros.isEmpty()) {
            println("A biblioteca está vazia.")
        } else {
            println("Livros na biblioteca:")
            for (livro in livros) {
                println(livro)
            }
        }
    }

    // Função para remover um livro da biblioteca
    fun removerLivro(titulo: String) {
        val livroRemovido = livros.find { it.titulo.equals(titulo, ignoreCase = true) }
        if (livroRemovido != null) {
            livros.remove(livroRemovido)
            println("Livro removido com sucesso!")
        } else {
            println("Livro não encontrado na biblioteca.")
        }
    }
}

// Função principal que interage com o usuário
fun main() {
    val scanner = Scanner(System.`in`)
    val biblioteca = Biblioteca()

    while (true) {
        println("Opções:")
        println("1 - Adicionar livro")
        println("2 - Visualizar livros")
        println("3 - Remover livro")
        println("0 - Sair")
        print("Escolha uma opção: ")

        when (scanner.nextInt()) {
            1 -> {
                print("Título do livro: ")
                val titulo = scanner.next()
                print("Autor do livro: ")
                val autor = scanner.next()
                print("Ano de publicação do livro: ")
                val anoPublicacao = scanner.nextInt()
                print("O livro possui número de páginas? (S/N): ")
                val resposta = scanner.next()
                if (resposta.equals("S", ignoreCase = true)) {
                    print("Número de páginas do livro: ")
                    val numPaginas = scanner.nextInt()
                    biblioteca.adicionarLivro(LivroComPaginas(titulo, autor, anoPublicacao, numPaginas))
                } else {
                    biblioteca.adicionarLivro(Livro(titulo, autor, anoPublicacao))
                }
            }
            2 -> biblioteca.visualizarLivros()
            3 -> {
                print("Título do livro a ser removido: ")
                val titulo = scanner.next()
                biblioteca.removerLivro(titulo)
            }
            0 -> return
            else -> println("Opção inválida.")
        }
        println()
    }
}
```

Este código começa com a importação da classe `Scanner` para lidar com a entrada do usuário. Em seguida, temos a definição da classe `Livro`, que serve como uma classe base para representar um livro com seus campos básicos: título, autor e ano de publicação. A classe `Livro` também possui um método `toString()` que retorna uma representação em string dos campos do livro.

Em seguida, temos a classe `LivroComPaginas`, que é uma classe derivada de `Livro`. Ela adiciona um campo adicional para o número de páginas do livro. A classe `LivroComPaginas` também sobrescreve o método `toString()` para adicionar o número de páginas na representação em string do livro.

A classe principal do programa é a `Biblioteca`. Ela possui um campo `livros`, que é uma lista mutável de objetos `Livro`. A classe `Biblioteca` também possui três métodos: `adicionarLivro()`, `visualizarLivros()` e `removerLivro()`. O método `adicionarLivro()` recebe um objeto `Livro` como parâmetro e o adiciona à lista de livros da biblioteca. O método `visualizarLivros()` exibe todos os livros presentes na biblioteca, caso haja algum. O método `removerLivro()` recebe o título de um livro como parâmetro e remove esse livro da lista, se ele existir.

A função `main()` é a função principal do programa. Ela cria um objeto `Scanner` para ler a entrada do usuário e um objeto `Biblioteca` para representar a biblioteca. Em seguida, entra em um loop infinito que exibe um menu de opções ao usuário. Dependendo da opção escolhida, o programa realiza a operação correspondente chamando os métodos apropriados da classe `Biblioteca`.

Espero que este exemplo atenda às suas expectativas de complexidade e diferenciação. Caso tenha alguma dúvida adicional ou precise de mais informações, estou à disposição para ajudar!