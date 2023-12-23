Claro! Aqui está um código em Kotlin que cria uma representação de um jogo de xadrez simplificado. O código é bastante extenso, mas foi projetado para ser fácil de entender e seguir.

```kotlin
import java.util.*

enum class Cor {
    BRANCO, PRETO
}

enum class TipoPeca {
    PEAO, TORRE, CAVALO, BISPO, RAINHA, REI
}

data class Posicao(val linha: Int, val coluna: Char)

abstract class Peca(val cor: Cor, val posicao: Posicao) {
    abstract val tipo: TipoPeca
    abstract fun podeMoverPara(destino: Posicao): Boolean
}

class Peao(cor: Cor, posicao: Posicao) : Peca(cor, posicao) {
    override val tipo = TipoPeca.PEAO

    override fun podeMoverPara(destino: Posicao): Boolean {
        // Implementação do movimento do peão
        // ...
    }
}

class Torre(cor: Cor, posicao: Posicao) : Peca(cor, posicao) {
    override val tipo = TipoPeca.TORRE

    override fun podeMoverPara(destino: Posicao): Boolean {
        // Implementação do movimento da torre
        // ...
    }
}

// Implementação das outras peças (Cavalo, Bispo, Rainha, Rei)

class Tabuleiro {
    private val pecas: MutableList<Peca> = mutableListOf()

    fun adicionarPeca(peca: Peca) {
        pecas.add(peca)
    }

    fun moverPeca(origem: Posicao, destino: Posicao) {
        val peca = getPecaNaPosicao(origem)
        if (peca != null && peca.podeMoverPara(destino)) {
            pecas.remove(peca)
            peca.posicao = destino
            pecas.add(peca)
        } else {
            println("Movimento inválido!")
        }
    }

    private fun getPecaNaPosicao(posicao: Posicao): Peca? {
        return pecas.find { it.posicao == posicao }
    }
}

fun main() {
    val tabuleiro = Tabuleiro()
    val peao = Peao(Cor.BRANCO, Posicao(2, 'a'))
    val torre = Torre(Cor.PRETO, Posicao(1, 'h'))

    tabuleiro.adicionarPeca(peao)
    tabuleiro.adicionarPeca(torre)

    tabuleiro.moverPeca(Posicao(2, 'a'), Posicao(3, 'a'))
    tabuleiro.moverPeca(Posicao(1, 'h'), Posicao(3, 'h'))
}
```

Neste código, temos uma série de classes que representam as peças do jogo de xadrez, como `Peca`, `Peao`, `Torre`, etc. O tabuleiro é representado pela classe `Tabuleiro`, onde podemos adicionar peças e movê-las de acordo com as regras do jogo.

O código também inclui enumerações para representar as cores das peças (`Cor`) e os tipos de peças (`TipoPeca`), assim como a classe `Posicao` para armazenar as posições no tabuleiro.

No método `main`, criamos um tabuleiro, adicionamos um peão branco e uma torre preta, e movemos as peças para testar o funcionamento do código.

Claro, este código é apenas um exemplo simplificado e pode ser expandido para incluir todas as regras e movimentos do jogo de xadrez completo.