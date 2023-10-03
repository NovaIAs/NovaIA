Claro! Aqui está um código complexo em Scala que implementa um algoritmo de busca em largura em um grafo:

```scala
import scala.collection.mutable.Queue

class Graph(val vertices: Int) {
  private val adjList = Array.fill[List[Int]](vertices)(Nil)

  def addEdge(u: Int, v: Int): Unit = {
    adjList(u) = v :: adjList(u)
    adjList(v) = u :: adjList(v)
  }

  def bfs(start: Int): Unit = {
    val visited = Array.fill[Boolean](vertices)(false)
    val queue = new Queue[Int]

    visited(start) = true
    queue.enqueue(start)

    while (queue.nonEmpty) {
      val currentVertex = queue.dequeue()
      print(s"$currentVertex ")

      val neighbors = adjList(currentVertex)
      for (neighbor <- neighbors) {
        if (!visited(neighbor)) {
          visited(neighbor) = true
          queue.enqueue(neighbor)
        }
      }
    }
  }
}

object Main extends App {
  val graph = new Graph(7)
  
  graph.addEdge(0, 1)
  graph.addEdge(0, 2)
  graph.addEdge(1, 3)
  graph.addEdge(1, 4)
  graph.addEdge(2, 5)
  graph.addEdge(2, 6)

  println("BFS traversal starting from vertex 0:")
  graph.bfs(0)
}
```

Neste código, temos uma classe `Graph` que representa um grafo não direcionado. A classe possui um método `addEdge` que adiciona uma aresta ao grafo. Em seguida, temos o método `bfs` que implementa a busca em largura (BFS) a partir de um determinado vértice.

No objeto `Main`, criamos uma instância do grafo com 7 vértices e adicionamos algumas arestas. Em seguida, chamamos o método `bfs` a partir do vértice 0 para realizar a travessia em largura do grafo.

O algoritmo BFS começa visitando o vértice inicial e, em seguida, visita todos os seus vizinhos. Os vizinhos não visitados são enfileirados para serem visitados posteriormente. Esse processo continua até que todos os vértices sejam visitados ou a fila esteja vazia.

No final, o código imprime a travessia em largura do grafo, começando a partir do vértice 0.