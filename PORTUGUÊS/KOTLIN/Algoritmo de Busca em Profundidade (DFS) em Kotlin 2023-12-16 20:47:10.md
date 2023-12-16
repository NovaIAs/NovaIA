Claro! Aqui está um código complexo em Kotlin que implementa um algoritmo de busca em profundidade (DFS) em um grafo direcionado:

```kotlin
data class Vertice(val nome: String)
data class Aresta(val origem: Vertice, val destino: Vertice)

class Grafo {
    private val vertices = mutableListOf<Vertice>()
    private val arestas = mutableListOf<Aresta>()

    fun adicionarVertice(nome: String) {
        val vertice = Vertice(nome)
        vertices.add(vertice)
    }

    fun adicionarAresta(origem: Vertice, destino: Vertice) {
        val aresta = Aresta(origem, destino)
        arestas.add(aresta)
    }

    fun buscaEmProfundidade(verticeInicial: Vertice) {
        val visitados = mutableSetOf<Vertice>()
        dfsRecursivo(verticeInicial, visitados)
    }

    private fun dfsRecursivo(vertice: Vertice, visitados: MutableSet<Vertice>) {
        visitados.add(vertice)
        println("Visitando o vértice: ${vertice.nome}")
        
        for (aresta in arestas) {
            if (aresta.origem == vertice && aresta.destino !in visitados) {
                dfsRecursivo(aresta.destino, visitados)
            }
        }
    }
}

fun main() {
    val grafo = Grafo()
    
    // Adicionando vértices ao grafo
    grafo.adicionarVertice("A")
    grafo.adicionarVertice("B")
    grafo.adicionarVertice("C")
    grafo.adicionarVertice("D")
    grafo.adicionarVertice("E")
    
    // Adicionando arestas ao grafo
    grafo.adicionarAresta(Vertice("A"), Vertice("B"))
    grafo.adicionarAresta(Vertice("A"), Vertice("C"))
    grafo.adicionarAresta(Vertice("B"), Vertice("D"))
    grafo.adicionarAresta(Vertice("C"), Vertice("D"))
    grafo.adicionarAresta(Vertice("D"), Vertice("E"))
    
    // Realizando a busca em profundidade a partir do vértice "A"
    grafo.buscaEmProfundidade(Vertice("A"))
}
```

Neste código, temos a definição das classes `Vertice` e `Aresta` que representam os componentes do grafo. Em seguida, temos a classe `Grafo`, que contém os métodos para adicionar vértices e arestas ao grafo, bem como o algoritmo de busca em profundidade (DFS).

O método `buscaEmProfundidade` inicia a busca a partir de um vértice inicial, criando um conjunto para armazenar os vértices já visitados e chamando o método `dfsRecursivo`.

O método `dfsRecursivo` é responsável por realizar a busca em profundidade de forma recursiva. Ele marca o vértice atual como visitado, imprime seu nome e, em seguida, itera sobre todas as arestas do grafo. Se uma aresta tiver como origem o vértice atual e seu destino ainda não tiver sido visitado, o método é chamado recursivamente para explorar esse vértice.

No método `main`, criamos um objeto `Grafo` e adicionamos vértices e arestas a ele. Em seguida, realizamos uma busca em profundidade a partir do vértice "A".

O resultado impresso será:

```
Visitando o vértice: A
Visitando o vértice: B
Visitando o vértice: D
Visitando o vértice: E
Visitando o vértice: C
```

Isso indica a ordem em que os vértices foram visitados durante a busca em profundidade.