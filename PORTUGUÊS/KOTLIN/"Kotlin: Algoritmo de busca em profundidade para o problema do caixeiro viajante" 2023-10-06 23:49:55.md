Claro! Aqui está um exemplo de um código complexo em Kotlin que implementa uma solução para o problema do caixeiro viajante usando o algoritmo de busca em profundidade:

```kotlin
class CaixeiroViajante(private val matrizDistancias: Array<IntArray>) {
    private var melhorCaminho = IntArray(matrizDistancias.size + 1)
    private var menorDistancia = Int.MAX_VALUE

    fun encontrarMelhorCaminho(): IntArray {
        val visitados = BooleanArray(matrizDistancias.size)
        val caminhoAtual = IntArray(matrizDistancias.size + 1)
        caminhoAtual[0] = 0
        visitados[0] = true

        buscarCaminho(0, 1, caminhoAtual, visitados)

        return melhorCaminho
    }

    private fun buscarCaminho(cidadeAtual: Int, nivel: Int, caminhoAtual: IntArray, visitados: BooleanArray) {
        if (nivel == matrizDistancias.size) {
            val distanciaAtual = calcularDistancia(caminhoAtual)
            if (distanciaAtual < menorDistancia) {
                caminhoAtual[nivel] = 0
                melhorCaminho = caminhoAtual.clone()
                menorDistancia = distanciaAtual
            }
            return
        }

        for (prx in 1 until matrizDistancias.size) {
            if (!visitados[prx]) {
                caminhoAtual[nivel] = prx
                visitados[prx] = true
                buscarCaminho(prx, nivel + 1, caminhoAtual, visitados)
                visitados[prx] = false
            }
        }
    }

    private fun calcularDistancia(caminhoAtual: IntArray): Int {
        var distancia = 0
        for (i in 0 until caminhoAtual.size - 1) {
            distancia += matrizDistancias[caminhoAtual[i]][caminhoAtual[i + 1]]
        }
        return distancia
    }
}

fun main() {
    val matrizDistancias = arrayOf(
        intArrayOf(0, 2, 9, 10),
        intArrayOf(1, 0, 6, 4),
        intArrayOf(15, 7, 0, 8),
        intArrayOf(6, 3, 12, 0)
    )

    val caixeiroViajante = CaixeiroViajante(matrizDistancias)
    val melhorCaminho = caixeiroViajante.encontrarMelhorCaminho()

    println("Melhor caminho encontrado:")
    for (cidade in melhorCaminho) {
        print("$cidade -> ")
    }
    println("0")
    println("Menor distância encontrada: ${caixeiroViajante.menorDistancia}")
}
```

Neste exemplo, a classe `CaixeiroViajante` representa o problema do caixeiro viajante. A matriz de distâncias é passada no construtor da classe. O método `encontrarMelhorCaminho` é responsável por iniciar a busca pelo melhor caminho.

O algoritmo de busca em profundidade é implementado no método privado `buscarCaminho`. Ele faz uso de recursão para testar todas as possibilidades de caminhos possíveis, mantendo controle das cidades visitadas. A cada iteração, é verificado se o caminho atual é o melhor caminho encontrado até o momento.

O método `calcularDistancia` é utilizado para calcular a distância total percorrida em um determinado caminho.

Na função `main`, uma matriz de distâncias é criada como exemplo. A classe `CaixeiroViajante` é instanciada com essa matriz e o melhor caminho encontrado é exibido junto com a menor distância percorrida.

Espero que este código complexo em Kotlin seja útil para você!