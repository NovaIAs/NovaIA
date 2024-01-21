```groovy
// Definimos una clase para representar un grafo.
class Grafo {
  // Un mapa para almacenar los vértices y sus conexiones.
  Map<String, List<String>> vertices = [:]

  // Método para añadir un vértice al grafo.
  void añadirVertice(String vertice) {
    vertices[vertice] = []
  }

  // Método para añadir una conexión entre dos vértices.
  void añadirConexion(String origen, String destino) {
    vertices[origen] << destino
  }

  // Método para obtener todos los vértices del grafo.
  List<String> obtenerVertices() {
    vertices.keySet().toList()
  }

  // Método para obtener todas las conexiones de un vértice.
  List<String> obtenerConexiones(String vertice) {
    vertices[vertice]
  }

  // Método para recorrer el grafo en amplitud (BFS).
  List<String> recorrerBFS(String inicio) {
    // Creamos una cola para almacenar los vértices a visitar.
    Queue<String> cola = new LinkedList<>()

    // Creamos un conjunto para almacenar los vértices visitados.
    Set<String> visitados = []

    // Añadimos el vértice inicial a la cola.
    cola.add(inicio)

    // Mientras haya vértices en la cola, los visitamos y añadimos sus conexiones a la cola.
    while (!cola.isEmpty()) {
      // Sacamos el primer vértice de la cola.
      String vertice = cola.poll()

      // Si el vértice no ha sido visitado, lo añadimos al conjunto de visitados y lo añadimos a la lista de vértices recorridos.
      if (!visitados.contains(vertice)) {
        visitados << vertice
        recorridos << vertice

        // Añadimos las conexiones del vértice a la cola.
        vertices[vertice].each { conexion ->
          if (!visitados.contains(conexion)) {
            cola.add(conexion)
          }
        }
      }
    }

    // Devolvemos la lista de vértices recorridos.
    recorridos
  }

  // Método para recorrer el grafo en profundidad (DFS).
  List<String> recorrerDFS(String inicio) {
    // Creamos una pila para almacenar los vértices a visitar.
    Stack<String> pila = new Stack<>()

    // Creamos un conjunto para almacenar los vértices visitados.
    Set<String> visitados = []

    // Añadimos el vértice inicial a la pila.
    pila.push(inicio)

    // Mientras haya vértices en la pila, los visitamos y añadimos sus conexiones a la pila.
    while (!pila.isEmpty()) {
      // Sacamos el primer vértice de la pila.
      String vertice = pila.pop()

      // Si el vértice no ha sido visitado, lo añadimos al conjunto de visitados y lo añadimos a la lista de vértices recorridos.
      if (!visitados.contains(vertice)) {
        visitados << vertice
        recorridos << vertice

        // Añadimos las conexiones del vértice a la pila.
        vertices[vertice].each { conexion ->
          if (!visitados.contains(conexion)) {
            pila.push(conexion)
          }
        }
      }
    }

    // Devolvemos la lista de vértices recorridos.
    recorridos
  }
}

// Creamos un grafo.
Grafo grafo = new Grafo()

// Añadimos algunos vértices al grafo.
grafo.añadirVertice("A")
grafo.añadirVertice("B")
grafo.añadirVertice("C")
grafo.añadirVertice("D")
grafo.añadirVertice("E")

// Añadimos algunas conexiones entre los vértices.
grafo.añadirConexion("A", "B")
grafo.añadirConexion("A", "C")
grafo.añadirConexion("B", "D")
grafo.añadirConexion("C", "D")
grafo.añadirConexion("C", "E")
grafo