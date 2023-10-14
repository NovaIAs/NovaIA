Claro, vou criar um código complexo em Scala para você. O código abaixo é um exemplo de um algoritmo de busca em largura (BFS) em um grafo direcionado.

```scala
import scala.collection.mutable.Queue

class Grafo(val vertices: Int) {
  private val adjacencia = Array.ofDim[Boolean](vertices, vertices)

  def adicionarAresta(origem: Int, destino: Int): Unit = {
    adjacencia(origem)(destino) = true
  }

  def bfs(inicio: Int): Unit = {
    val visitado = Array.fill[Boolean](vertices)(false)
    val fila = Queue[Int]()

    visitado(inicio) = true
    fila.enqueue(inicio)

    while (fila.nonEmpty) {
      val verticeAtual = fila.dequeue()
      print(verticeAtual + " ")

      for (i <- 0 until vertices) {
        if (adjacencia(verticeAtual)(i) && !visitado(i)) {
          visitado(i) = true
          fila.enqueue(i)
        }
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val grafo = new Grafo(6)

    grafo.adicionarAresta(0, 1)
    grafo.adicionarAresta(0, 2)
    grafo.adicionarAresta(1, 3)
    grafo.adicionarAresta(1, 4)
    grafo.adicionarAresta(2, 4)
    grafo.adicionarAresta(3, 4)
    grafo.adicionarAresta(3, 5)
    grafo.adicionarAresta(4, 5)

    println("Ordem de visita (BFS):")
    grafo.bfs(0)
  }
}
```

Este código contém uma classe `Grafo`, que representa o grafo direcionado e uma função `bfs` para realizar a busca em largura a partir de um vértice inicial. O método `adicionarAresta` é utilizado para adicionar arestas entre os vértices do grafo.

Na classe `Main`, criamos uma instância do grafo e adicionamos as arestas necessárias para representar o grafo. Em seguida, chamamos o método `bfs` passando o vértice inicial como parâmetro.

A busca em largura é um algoritmo utilizado para percorrer ou buscar elementos em um grafo. Neste exemplo, a ordem de visita dos vértices é impressa no console.