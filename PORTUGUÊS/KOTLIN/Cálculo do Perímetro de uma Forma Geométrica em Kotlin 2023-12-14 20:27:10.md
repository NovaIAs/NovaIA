Claro! Aqui está um código complexo em Kotlin que usa alguns recursos avançados da linguagem:

```kotlin
import kotlin.math.sqrt

data class Ponto(val x: Double, val y: Double)

class FormaGeometrica(val pontos: List<Ponto>) {
    fun calcularPerimetro(): Double {
        var perimetro = 0.0
        for (i in 0 until pontos.size - 1) {
            val pontoAtual = pontos[i]
            val pontoProximo = pontos[i + 1]
            val distancia = calcularDistancia(pontoAtual, pontoProximo)
            perimetro += distancia
        }
        perimetro += calcularDistancia(pontos.last(), pontos.first()) // Adiciona a distância do último ponto ao primeiro
        return perimetro
    }

    private fun calcularDistancia(ponto1: Ponto, ponto2: Ponto): Double {
        val dx = ponto2.x - ponto1.x
        val dy = ponto2.y - ponto1.y
        return sqrt(dx * dx + dy * dy)
    }
}

fun main() {
    val pontos = listOf(
        Ponto(0.0, 0.0),
        Ponto(0.0, 5.0),
        Ponto(5.0, 5.0),
        Ponto(5.0, 0.0)
    )

    val formaGeometrica = FormaGeometrica(pontos)
    val perimetro = formaGeometrica.calcularPerimetro()
    println("O perímetro da forma geométrica é: $perimetro")
}
```

Neste código, criamos duas classes: `Ponto` e `FormaGeometrica`. A classe `Ponto` representa um ponto no plano cartesiano, com as coordenadas `x` e `y`.

A classe `FormaGeometrica` representa uma forma geométrica definida por uma lista de pontos. Ela possui um método `calcularPerimetro()` que calcula o perímetro da forma, somando as distâncias entre cada par de pontos. A função `calcularDistancia()` é um método auxiliar privado que calcula a distância entre dois pontos usando a fórmula da distância euclidiana.

No `main()`, criamos uma lista de pontos que representam um quadrado, e em seguida, criamos uma instância da classe `FormaGeometrica` com esses pontos. Chamamos o método `calcularPerimetro()` para obter o perímetro da forma e o mostramos na saída.

Esse código exemplifica o uso de classes, métodos, loops, operações matemáticas, entre outros recursos do Kotlin.