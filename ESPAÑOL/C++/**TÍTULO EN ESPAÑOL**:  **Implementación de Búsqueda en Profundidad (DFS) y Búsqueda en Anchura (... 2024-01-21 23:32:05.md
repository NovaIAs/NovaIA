```c++
// Librerías necesarias
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

// Definición de la clase Grafo
class Grafo {
private:
  // Lista de adyacencia para representar el grafo
  std::vector<std::vector<int>> adjList;
  // Vector que indica si un vértice ha sido visitado o no
  std::vector<bool> visitado;

public:
  // Constructor del grafo
  Grafo(int numVertices) {
    adjList.resize(numVertices);
    visitado.resize(numVertices, false);
  }

  // Método para agregar una arista al grafo
  void agregarArista(int u, int v) {
    adjList[u].push_back(v);
  }

  // Método para realizar una búsqueda en profundidad (DFS) en el grafo
  void dfs(int nodoActual) {
    // Marca el nodo actual como visitado
    visitado[nodoActual] = true;

    // Imprime el nodo actual
    std::cout << nodoActual << " ";

    // Recorre los vecinos del nodo actual
    for (int vecino : adjList[nodoActual]) {
      // Si el vecino no ha sido visitado, realiza una llamada recursiva a dfs()
      if (!visitado[vecino]) {
        dfs(vecino);
      }
    }
  }

  // Método para realizar una búsqueda en anchura (BFS) en el grafo
  void bfs(int nodoInicial) {
    // Cola para almacenar los nodos a visitar
    std::queue<int> cola;

    // Marca el nodo inicial como visitado y lo añade a la cola
    visitado[nodoInicial] = true;
    cola.push(nodoInicial);

    // Mientras la cola no esté vacía
    while (!cola.empty()) {
      // Desapila el primer nodo de la cola
      int nodoActual = cola.front();
      cola.pop();

      // Imprime el nodo actual
      std::cout << nodoActual << " ";

      // Recorre los vecinos del nodo actual
      for (int vecino : adjList[nodoActual]) {
        // Si el vecino no ha sido visitado, lo marca como visitado y lo añade a la cola
        if (!visitado[vecino]) {
          visitado[vecino] = true;
          cola.push(vecino);
        }
      }
    }
  }
};

// Función principal
int main() {
  // Crea un grafo con 6 vértices
  Grafo grafo(6);

  // Agrega aristas al grafo
  grafo.agregarArista(0, 1);
  grafo.agregarArista(0, 2);
  grafo.agregarArista(1, 2);
  grafo.agregarArista(1, 3);
  grafo.agregarArista(2, 4);
  grafo.agregarArista(2, 5);
  grafo.agregarArista(3, 4);
  grafo.agregarArista(4, 5);

  // Imprime el grafo mediante DFS
  std::cout << "Búsqueda en profundidad (DFS): ";
  grafo.dfs(0);
  std::cout << std::endl;

  // Restablece los flags de visitado
  std::fill(grafo.visitado.begin(), grafo.visitado.end(), false);

  // Imprime el grafo mediante BFS
  std::cout << "Búsqueda en anchura (BFS): ";
  grafo.bfs(0);
  std::cout << std::endl;

  return 0;
}
```

**Explicación del código:**

* La clase `Grafo` representa un grafo no dirigido. Tiene un constructor que toma el número de vértices en el grafo y crea estructuras de datos para almacenar la lista de adyacencia y el vector de visitado.
* El método `agregarArista()` agrega una arista al grafo uniendo dos vértices especificados.
* El método `dfs()` realiza una búsqueda en profundidad (DFS) en el grafo a partir de un nodo especificado.
* El método `bfs()` realiza una búsqueda en anchura (BFS) en el grafo a partir de un nodo especificado.
* La función `main()` crea un grafo con 6 vértices y agrega aristas para formar un grafo conectado. Luego llama a los métodos `dfs()` y `bfs()` para imprimir el grafo mediante DFS y BFS, respectivamente.