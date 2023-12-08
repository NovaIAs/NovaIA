Claro! Aqui está um exemplo de um código complexo em Kotlin que mostra como criar e manipular grafos direcionados usando uma implementação de lista de adjacências:

```kotlin
class GrafoDirecionado<T> {

    private val vertices = mutableMapOf<T, MutableList<T>>()

    fun adicionarVertice(vertice: T) {
        vertices[vertice] = mutableListOf()
    }

    fun adicionarAresta(origem: T, destino: T) {
        if (!vertices.containsKey(origem) || !vertices.containsKey(destino)) {
            throw IllegalArgumentException("Os vértices fornecidos não existem no grafo.")
        }
        vertices[origem]?.add(destino)
    }

    fun removerAresta(origem: T, destino: T) {
        if (!vertices.containsKey(origem) || !vertices.containsKey(destino)) {
            throw IllegalArgumentException("Os vértices fornecidos não existem no grafo.")
        }
        vertices[origem]?.remove(destino)
    }

    fun verificarExistenciaVertice(vertice: T): Boolean {
        return vertices.containsKey(vertice)
    }

    fun verificarExistenciaAresta(origem: T, destino: T): Boolean {
        if (!vertices.containsKey(origem) || !vertices.containsKey(destino)) {
            throw IllegalArgumentException("Os vértices fornecidos não existem no grafo.")
        }
        return vertices[origem]?.contains(destino) ?: false
    }

    override fun toString(): String {
        val sb = StringBuilder()
        for ((vertice, arestas) in vertices) {
            sb.append("$vertice -> ")
            for (destino in arestas) {
                sb.append("$destino ")
            }
            sb.append("\n")
        }
        return sb.toString()
    }
}

fun main() {
    val grafo = GrafoDirecionado<String>()

    grafo.adicionarVertice("A")
    grafo.adicionarVertice("B")
    grafo.adicionarVertice("C")
    grafo.adicionarVertice("D")

    grafo.adicionarAresta("A", "B")
    grafo.adicionarAresta("B", "C")
    grafo.adicionarAresta("C", "D")
    grafo.adicionarAresta("D", "A")

    println("Grafo:")
    println(grafo)

    println("Removendo a aresta A -> B")
    grafo.removerAresta("A", "B")

    println("Verificando existência da aresta A -> B: ${grafo.verificarExistenciaAresta("A", "B")}")

    println("Grafo após remoção:")
    println(grafo)
}
```

Neste exemplo, criei uma classe `GrafoDirecionado<T>` que usa uma implementação de lista de adjacências para representar um grafo direcionado genérico. Os vértices são armazenados em um `MutableMap`, onde as chaves são os vértices e os valores são listas de vértices adjacentes. 

O código inclui métodos para adicionar e remover vértices e arestas, verificar a existência de vértices e arestas, e um método `toString()` que imprime o grafo no formato "origem -> destino1 destino2 ..." para facilitar a visualização.

No método `main()`, criei um exemplo de uso do grafo, adicionando vértices (A, B, C, D) e arestas (A -> B, B -> C, C -> D, D -> A). Em seguida, removi a aresta A -> B e verifiquei a existência da mesma após a remoção. Por fim, imprimo o grafo resultante para verificar as modificações.
