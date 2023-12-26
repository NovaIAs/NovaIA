Claro! Aqui está um exemplo de um código complexo em Kotlin, que utiliza várias funcionalidades da linguagem:

```kotlin
class JogoDeCartas(val nome: String, val jogadores: List<String>) {
    private val baralho = mutableListOf<String>()

    init {
        popularBaralho()
        embaralhar()
    }

    private fun popularBaralho() {
        val naipes = listOf("Copas", "Espadas", "Ouros", "Paus")
        val valores = listOf("Ás", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Valete", "Dama", "Rei")

        for (naipe in naipes) {
            for (valor in valores) {
                baralho.add("$valor de $naipe")
            }
        }
    }

    private fun embaralhar() {
        baralho.shuffle()
    }

    fun distribuirCartas() {
        val cartasPorJogador = baralho.size / jogadores.size
        var indexCarta = 0

        for (jogador in jogadores) {
            val cartasJogador = baralho.subList(indexCarta, indexCarta + cartasPorJogador)
            indexCarta += cartasPorJogador

            println("Cartas do jogador $jogador:")
            for (carta in cartasJogador) {
                println(carta)
            }

            println()
        }
    }
}

fun main() {
    val jogo = JogoDeCartas("Pôquer", listOf("Alice", "Bob", "Carol", "Dave"))
    jogo.distribuirCartas()
}
```

Neste exemplo, criamos uma classe `JogoDeCartas` que representa um jogo de cartas. Ela possui um nome e uma lista de jogadores. O código utiliza os conceitos de classes, propriedades, inicializadores e funções.

No inicializador (`init`), chamamos os métodos `popularBaralho()` e `embaralhar()`. O método `popularBaralho()` cria um baralho de cartas, combinando naipes e valores. O método `embaralhar()` embaralha as cartas do baralho.

A função `distribuirCartas()` distribui as cartas do baralho para os jogadores. Ela calcula quantas cartas cada jogador receberá, com base no tamanho do baralho e na quantidade de jogadores. Em seguida, itera sobre os jogadores e exibe suas respectivas cartas.

No método `main()`, criamos uma instância da classe `JogoDeCartas` chamada `jogo`, com o nome "Pôquer" e uma lista de jogadores. Em seguida, chamamos o método `distribuirCartas()` para iniciar o jogo e exibir as cartas distribuídas para cada jogador.

Este código cria um jogo de cartas simples em Kotlin, mostrando como utilizar classes, propriedades, inicializadores e funções para organizar e manipular os dados do jogo.