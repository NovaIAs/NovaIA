Claro! Vou criar um código complexo em Kotlin para você. Aqui está um exemplo de um programa que implementa um algoritmo de busca em profundidade (DFS) em um grafo direcionado. O algoritmo visita todos os nós do grafo e imprime o resultado no console. Vou explicar o código passo a passo:

```kotlin
class Grafo(val numVertices: Int) {
    private val adjacencias: Array<MutableList<Int>> = Array(numVertices) { mutableListOf() }

    fun adicionarAresta(origem: Int, destino: Int) {
        adjacencias[origem].add(destino)
    }

    fun dfs(v: Int, visitados: BooleanArray) {
        visitados[v] = true
        print("$v ")

        val iterator = adjacencias[v].iterator()
        while (iterator.hasNext()) {
            val proximo = iterator.next()
            if (!visitados[proximo]) {
                dfs(proximo, visitados)
            }
        }
    }

    fun dfs() {
        val visitados = BooleanArray(numVertices)
        for (i in 0 until numVertices) {
            if (!visitados[i]) {
                dfs(i, visitados)
            }
        }
    }
}

fun main() {
    val g = Grafo(5)

    g.adicionarAresta(0, 1)
    g.adicionarAresta(0, 2)
    g.adicionarAresta(1, 3)
    g.adicionarAresta(2, 4)

    println("Resultado da busca em profundidade:")
    g.dfs()
}
```

1. A classe `Grafo` representa a estrutura de dados do grafo e contém uma matriz de listas de adjacências, onde cada lista representa os vértices adjacentes de um determinado vértice.

2. O método `adicionarAresta` é usado para adicionar uma aresta ao grafo, especificando a origem e o destino.

3. O método `dfs` implementa o algoritmo de busca em profundidade. Ele recebe um vértice inicial (`v`) e um array de booleanos (`visitados`) que indica se cada vértice já foi visitado. O método utiliza uma abordagem recursiva para visitar todos os vértices alcançáveis a partir do vértice inicial, marcando-os como visitados e imprimindo o valor do vértice no console.

4. O método `dfs` sem parâmetros é um wrapper que percorre todos os vértices do grafo e chama o método `dfs` para cada vértice não visitado.

5. Na função `main`, criamos uma instância do grafo com 5 vértices. Em seguida, adicionamos algumas arestas ao grafo.

6. Por fim, chamamos o método `dfs` para realizar a busca em profundidade no grafo e imprimir o resultado no console.

Espero que isso atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais alguma coisa, é só me avisar.