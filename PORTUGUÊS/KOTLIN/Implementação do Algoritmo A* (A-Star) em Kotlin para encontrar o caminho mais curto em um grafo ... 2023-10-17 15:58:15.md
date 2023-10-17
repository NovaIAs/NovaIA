Claro! Aqui está um código complexo em Kotlin que implementa um algoritmo de busca chamado A* (A-star). O algoritmo é usado para encontrar o caminho mais curto entre dois pontos em um grafo ponderado.

```
import java.util.*

class AStar {

    data class Node(val id: Int, val heuristic: Int)

    data class Edge(val from: Node, val to: Node, val weight: Int)

    fun findShortestPath(start: Node, goal: Node, edges: List<Edge>): List<Node> {
        val openSet = PriorityQueue<Node>(compareBy { it.heuristic })
        val cameFrom = mutableMapOf<Node, Node>()
        val gScore = mutableMapOf<Node, Int>().withDefault { Int.MAX_VALUE }
        val fScore = mutableMapOf<Node, Int>().withDefault { Int.MAX_VALUE }

        gScore[start] = 0
        fScore[start] = start.heuristic
        openSet.add(start)

        while (openSet.isNotEmpty()) {
            val current = openSet.poll()

            if (current == goal) {
                return reconstructPath(cameFrom, current)
            }

            for (edge in edges.filter { it.from == current }) {
                val neighbor = edge.to
                val tentativeGScore = gScore[current]!! + edge.weight

                if (tentativeGScore < gScore[neighbor]!!) {
                    cameFrom[neighbor] = current
                    gScore[neighbor] = tentativeGScore
                    fScore[neighbor] = tentativeGScore + neighbor.heuristic

                    if (!openSet.contains(neighbor)) {
                        openSet.add(neighbor)
                    }
                }
            }
        }

        return emptyList()
    }

    private fun reconstructPath(cameFrom: Map<Node, Node>, current: Node): List<Node> {
        val path = mutableListOf<Node>()
        var node: Node? = current

        while (node != null) {
            path.add(node)
            node = cameFrom[node]
        }

        return path.reversed()
    }
}

fun main() {
    val nodeA = AStar.Node(1, 10)
    val nodeB = AStar.Node(2, 5)
    val nodeC = AStar.Node(3, 8)
    val nodeD = AStar.Node(4, 3)
    val nodeE = AStar.Node(5, 2)
    val nodeF = AStar.Node(6, 0)

    val edges = listOf(
        AStar.Edge(nodeA, nodeB, 4),
        AStar.Edge(nodeA, nodeC, 2),
        AStar.Edge(nodeB, nodeC, 1),
        AStar.Edge(nodeB, nodeD, 5),
        AStar.Edge(nodeC, nodeD, 8),
        AStar.Edge(nodeC, nodeE, 10),
        AStar.Edge(nodeD, nodeF, 6),
        AStar.Edge(nodeE, nodeF, 3)
    )

    val astar = AStar()
    val shortestPath = astar.findShortestPath(nodeA, nodeF, edges)
    println("Caminho mais curto: $shortestPath")
}
```

Este código implementa a classe `AStar` que contém o método `findShortestPath` que recebe um nó inicial, um nó final e uma lista de arestas que representam o grafo. Ele utiliza uma fila de prioridade para armazenar os nós abertos, um mapa para armazenar os nós visitados anteriormente, um mapa para armazenar os custos acumulados para cada nó, e um mapa para armazenar a estimativa de custos para cada nó.

O algoritmo A* é executado dentro de um loop enquanto a fila de prioridade `openSet` não estiver vazia. A cada iteração, o algoritmo seleciona o nó de menor custo estimado `current` da fila `openSet`. Se `current` for o nó objetivo, o caminho mais curto é reconstruído utilizando o método `reconstructPath` e é retornado.

Caso contrário, o algoritmo percorre todas as arestas que saem do `current` e atualiza os custos acumulados e estimados para cada vizinho. Se o custo acumulado até o vizinho for menor que o custo já registrado, o vizinho é adicionado ao `openSet` para ser explorado posteriormente.

No final, o programa principal cria alguns nós e arestas de exemplo e chama o método `findShortestPath` para encontrar o caminho mais curto entre os nós `nodeA` e `nodeF`. O resultado é impresso no console.