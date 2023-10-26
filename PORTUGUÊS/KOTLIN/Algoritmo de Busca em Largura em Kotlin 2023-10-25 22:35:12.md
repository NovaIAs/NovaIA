Claro! Aqui está um código complexo em Kotlin que realiza um algoritmo de busca em largura em um grafo:

```kotlin
import java.util.LinkedList

class Grafo(val numVertices: Int) {
    private val adjacencias: Array<LinkedList<Int>> = Array(numVertices) { LinkedList() }

    fun adicionarAresta(v: Int, w: Int) {
        adjacencias[v].add(w)
    }

    fun buscaEmLargura(verticeInicial: Int) {
        val visitado = BooleanArray(numVertices)
        val fila = LinkedList<Int>()

        visitado[verticeInicial] = true
        fila.add(verticeInicial)

        while (fila.isNotEmpty()) {
            val verticeAtual = fila.poll()
            print("$verticeAtual ")

            val iterator = adjacencias[verticeAtual].listIterator()
            while (iterator.hasNext()) {
                val adjacente = iterator.next()
                if (!visitado[adjacente]) {
                    visitado[adjacente] = true
                    fila.add(adjacente)
                }
            }
        }
    }
}

fun main() {
    val grafo = Grafo(6)

    grafo.adicionarAresta(0, 1)
    grafo.adicionarAresta(0, 2)
    grafo.adicionarAresta(1, 3)
    grafo.adicionarAresta(1, 4)
    grafo.adicionarAresta(2, 4)
    grafo.adicionarAresta(3, 4)
    grafo.adicionarAresta(3, 5)
    grafo.adicionarAresta(4, 5)

    println("Busca em Largura a partir do vértice 0:")
    grafo.buscaEmLargura(0)
}
```

Neste código, estamos implementando a classe `Grafo`, que representa um grafo com um número específico de vértices. Possui um método `adicionarAresta` que adiciona uma aresta entre dois vértices. Além disso, temos o método `buscaEmLargura`, que realiza a busca em largura a partir de um vértice inicial.

Na função `main`, criamos um grafo com 6 vértices e adicionamos algumas arestas. Em seguida, chamamos o método `buscaEmLargura` a partir do vértice 0 para demonstrar o algoritmo em ação.

A busca em largura é um algoritmo que visita todos os vértices de um grafo de forma iterativa, explorando todos os vértices "vizinhos" antes de avançar para os próximos níveis. É um algoritmo bastante utilizado para encontrar o caminho mais curto entre dois vértices, ou para percorrer todos os vértices do grafo de maneira sistemática.