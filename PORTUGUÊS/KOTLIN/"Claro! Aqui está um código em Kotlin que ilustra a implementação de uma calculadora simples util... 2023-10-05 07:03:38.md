Claro! Aqui está um código complexo em Kotlin que envolve o uso de classes e interfaces para criar um sistema de gerenciamento de biblioteca. Vou explicar cada parte do código para facilitar o entendimento:

```kotlin
import java.util.*

// Classe representando um livro na biblioteca
data class Livro(val titulo: String, val autor: String, val anoPublicacao: Int)

// Interface para definir métodos comuns para todas as bibliotecas
interface Biblioteca {
    fun adicionarLivro(livro: Livro)
    fun removerLivro(livro: Livro)
    fun buscarLivroPorTitulo(titulo: String): Livro?
    fun buscarLivroPorAutor(autor: String): List<Livro>
}

// Classe que implementa a interface Biblioteca
class BibliotecaImpl : Biblioteca {
    private val livros = mutableListOf<Livro>()

    override fun adicionarLivro(livro: Livro) {
        livros.add(livro)
        println("Livro adicionado: ${livro.titulo}")
    }

    override fun removerLivro(livro: Livro) {
        livros.remove(livro)
        println("Livro removido: ${livro.titulo}")
    }

    override fun buscarLivroPorTitulo(titulo: String): Livro? {
        return livros.find { it.titulo == titulo }
    }

    override fun buscarLivroPorAutor(autor: String): List<Livro> {
        return livros.filter { it.autor == autor }
    }
}

fun main() {
    val biblioteca: Biblioteca = BibliotecaImpl()

    val livro1 = Livro("Dom Casmurro", "Machado de Assis", 1899)
    val livro2 = Livro("Crime e Castigo", "Fiódor Dostoiévski", 1866)
    val livro3 = Livro("1984", "George Orwell", 1949)

    biblioteca.adicionarLivro(livro1)
    biblioteca.adicionarLivro(livro2)
    biblioteca.adicionarLivro(livro3)

    val livroEncontrado = biblioteca.buscarLivroPorTitulo("1984")
    livroEncontrado?.let {
        println("Livro encontrado: ${it.titulo} (${it.autor})")
    }

    val livrosDoAutor = biblioteca.buscarLivroPorAutor("Machado de Assis")
    println("Livros do autor:")
    livrosDoAutor.forEach {
        println("${it.titulo} (${it.anoPublicacao})")
    }

    biblioteca.removerLivro(livro2)
}
```

Explicação do código:

1. Começamos importando a classe `java.util.*`, que é necessária para usar as classes de coleções do Java.

2. Em seguida, definimos a classe `Livro` que representa um livro na biblioteca. Ele tem três propriedades: `titulo`, `autor` e `anoPublicacao`. Usamos a palavra-chave `data` para automaticamente gerar os métodos `equals`, `hashCode`, `toString`, entre outros.

3. Em seguida, definimos a interface `Biblioteca`, que define os métodos comuns que todas as bibliotecas devem implementar. Ela possui quatro métodos: `adicionarLivro`, `removerLivro`, `buscarLivroPorTitulo` e `buscarLivroPorAutor`.

4. A classe `BibliotecaImpl` implementa a interface `Biblioteca` e fornece uma implementação concreta para os métodos. Ela usa uma lista mutável `livros` para armazenar os livros adicionados.

5. No método `main()`, criamos uma instância da classe `BibliotecaImpl` e adicionamos alguns livros usando o método `adicionarLivro`.

6. Em seguida, usamos o método `buscarLivroPorTitulo` para procurar um livro com o título "1984". Se o livro for encontrado, imprimimos uma mensagem com o título e o autor.

7. Depois, usamos o método `buscarLivroPorAutor` para procurar todos os livros de um autor específico e imprimimos seus títulos e anos de publicação.

8. Por fim, usamos o método `removerLivro` para remover um livro da biblioteca.

Esse código cria uma estrutura básica para gerenciar uma biblioteca e demonstra o uso de classes, interfaces, listas mutáveis e operações com objetos em Kotlin.