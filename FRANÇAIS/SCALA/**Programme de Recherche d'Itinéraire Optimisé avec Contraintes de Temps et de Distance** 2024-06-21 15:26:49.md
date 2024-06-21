**Programme de recherche d'itinéraire optimisé avec contraintes en temps et distance**

**Objectif:** Développer un moteur de recherche d'itinéraire qui calcule les itinéraires optimaux en tenant compte de plusieurs contraintes, notamment les temps de trajet, les distances et les restrictions de temps.

**Code:**

```scala
import scala.collection.mutable.ListBuffer

object Pathfinder {

  // Modèle de données représentant un nœud dans le graphe
  case class Node(id: Int, lat: Double, lon: Double)

  // Modèle de données représentant un arc dans le graphe
  case class Edge(src: Int, dst: Int, weight: Double)

  // Classe représentant le graphe
  class Graph(nodes: List[Node], edges: List[Edge]) {

    // Méthode Dijkstra pour trouver le chemin le plus court entre deux nœuds
    def dijkstra(src: Int, dst: Int): ListBuffer[Int] = {
      val dist = Array.fill(nodes.length)(Double.PositiveInfinity)
      val prev = Array.fill(nodes.length)(-1)
      dist(src) = 0.0

      val unvisited = new ListBuffer[Int]
      unvisited ++= nodes.indices

      while (unvisited.nonEmpty) {
        val u = unvisited.minBy(dist)
        unvisited -= u

        if (u == dst) {
          val path = new ListBuffer[Int]
          var v = u
          while (v != src) {
            path += v
            v = prev(v)
          }
          path += src
          path.reverse
        }

        for (edge <- edges.filter(_.src == u)) {
          val alt = dist(u) + edge.weight
          if (alt < dist(edge.dst)) {
            dist(edge.dst) = alt
            prev(edge.dst) = u
          }
        }
      }

      throw new RuntimeException("Aucun chemin trouvé")  // Aucun chemin trouvé
    }

    // Méthode A* pour trouver le chemin le plus court satisfaisant les contraintes de temps et de distance
    def aStar(src: Int, dst: Int, maxTime: Double, maxDist: Double): ListBuffer[Int] = {
      val gScore = Array.fill(nodes.length)(Double.PositiveInfinity)
      val fScore = Array.fill(nodes.length)(Double.PositiveInfinity)
      val prev = Array.fill(nodes.length)(-1)

      gScore(src) = 0.0
      fScore(src) = heuristic(src, dst)

      val openSet = new ListBuffer[Int]
      openSet += src

      while (openSet.nonEmpty) {
        val current = openSet.minBy(fScore)
        openSet -= current

        if (current == dst) {
          val path = new ListBuffer[Int]
          var v = current
          while (v != src) {
            path += v
            v = prev(v)
          }
          path += src
          path.reverse
        }

        for (edge <- edges.filter(_.src == current)) {
          if (gScore(current) + edge.weight > maxDist) {
            continue
          }
          if (gScore(current) + edge.weight > maxTime) {
            continue
          }

          val tentativeGScore = gScore(current) + edge.weight
          if (tentativeGScore < gScore(edge.dst)) {
            prev(edge.dst) = current
            gScore(edge.dst) = tentativeGScore
            fScore(edge.dst) = gScore(edge.dst) + heuristic(edge.dst, dst)
            if (!openSet.contains(edge.dst)) {
              openSet += edge.dst
            }
          }
        }
      }

      throw new RuntimeException("Aucun chemin trouvé")  // Aucun chemin trouvé
    }

    // Fonction heuristique pour A*
    def heuristic(src: Int, dst: Int): Double = {
      val latDiff = Math.abs(nodes(src).lat - nodes(dst).lat)
      val lonDiff = Math.abs(nodes(src).lon - nodes(dst).lon)
      Math.sqrt(latDiff * latDiff + lonDiff * lonDiff)
    }
  }

  // Fonction principale pour tester le programme
  def main(args: Array[String]): Unit = {
    val nodes = List(
      Node(0, 48.8582, 2.2945),
      Node(1, 48.8585, 2.2926),
      Node(2, 48.8575, 2.2931),
      Node(3, 48.8580, 2.2911),
      Node(4, 48.8590, 2.2907)
    )

    val edges = List(
      Edge(0, 1, 0.1),
      Edge(1, 2, 0.1),
      Edge(2, 3, 0.1),
      Edge(3, 4, 0.1),
      Edge(0, 4, 0.2)
    )

    val graph = new Graph(nodes, edges)

    // Exemple d'utilisation de Dijkstra
    val dijkstraPath = graph.dijkstra(0, 4)
    println("Chemin le plus court (Dijkstra): " + dijkstraPath)

    // Exemple d'utilisation de A*
    val aStarPath = graph.aStar(0, 4, 0.5, 0.5)
    println("Chemin le plus court (A*): " + aStarPath)
  }
}
```

**Explication du code:**

* Le code implémente l'algorithme de Dijkstra pour trouver le chemin le plus court entre deux nœuds dans un graphe pondéré.
* Il implémente également l'algorithme A* pour trouver le chemin le plus court satisfaisant les contraintes de temps et de distance.
* Le graphe est représenté à l'aide d'objets `Node` et `Edge`.
* La fonction `heuristic()` calcule l'heuristique à utiliser par A*.
* La fonction `main()` est le point d'entrée du programme, qui crée un graphe d'exemple et exécute les algorithmes de Dijkstra et A* pour trouver des chemins entre des nœuds.