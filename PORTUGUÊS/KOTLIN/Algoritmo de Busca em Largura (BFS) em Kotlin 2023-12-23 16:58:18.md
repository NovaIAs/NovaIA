Claro! Aqui está um exemplo de código complexo em Kotlin, que demonstra um algoritmo de busca em largura (BFS) em um grafo direcionado:

```kotlin
import java.util.*

class Grafo(val vertices: Int) {
    private val adjacencias: Array<LinkedList<Int>> = Array(vertices) { LinkedList<Int>() }

    fun adicionarAresta(origem: Int, destino: Int) {
        adjacencias[origem].add(destino)
    }

    fun buscaEmLargura(origem: Int) {
        val visitados = BooleanArray(vertices)
        val fila: Queue<Int> = LinkedList()
        visitados[origem] = true
        fila.add(origem)

        while (fila.isNotEmpty()) {
            val vertice = fila.poll()
            print("$vertice ")

            val adjacenciasVertice = adjacencias[vertice]
            for (adjacencia in adjacenciasVertice) {
                if (!visitados[adjacencia]) {
                    visitados[adjacencia] = true
                    fila.add(adjacencia)
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

Neste código, começamos definindo a classe `Grafo`, que representa um grafo direcionado através de uma matriz de adjacências. A função `adicionarAresta` é responsável por adicionar uma aresta entre dois vértices.

A função `buscaEmLargura` implementa o algoritmo de busca em largura. Primeiro, utilizamos um array de booleanos chamado `visitados` para controlar quais vértices já foram visitados durante a busca. Em seguida, criamos uma fila vazia, onde adicionamos o vértice de origem.

Dentro do loop principal, removemos um vértice da fila (através da função `poll`) e o imprimimos. Em seguida, percorremos todas as adjacências desse vértice e, se alguma adjacência ainda não tiver sido visitada, a marcamos como visitada e a adicionamos na fila.

No método `main`, criamos um objeto `grafo` e adicionamos algumas arestas a ele. Em seguida, chamamos o método `buscaEmLargura` a partir do vértice 0 e imprimimos o resultado.

Espero que isso atenda às suas expectativas!