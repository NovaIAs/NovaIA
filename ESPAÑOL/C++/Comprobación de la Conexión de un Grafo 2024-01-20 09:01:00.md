```c++
// Incluimos las bibliotecas necesarias
#include <iostream>
#include <vector>
#include <algorithm>

// Definimos una estructura para representar un grafo
struct grafo {
  // Número de vértices del grafo
  int V;

  // Lista de adyacencia del grafo
  std::vector<std::vector<int>> adj;

  // Constructor del grafo
  grafo(int V) : V(V), adj(V) {}

  // Añadimos una arista al grafo
  void añadirArista(int u, int v) {
    adj[u].push_back(v);
    adj[v].push_back(u);
  }

  // Realizamos una búsqueda en profundidad del grafo
  void dfs(int u, std::vector<bool>& visitado) {
    // Marcamos el vértice como visitado
    visitado[u] = true;

    // Recorremos los vértices adyacentes
    for (auto v : adj[u]) {
      // Si el vértice no ha sido visitado, realizamos una búsqueda en profundidad
      if (!visitado[v]) {
        dfs(v, visitado);
      }
    }
  }

  // Comprobamos si el grafo es conexo
  bool esConexo() {
    // Creamos un vector de booleanos para marcar los vértices visitados
    std::vector<bool> visitado(V, false);

    // Realizamos una búsqueda en profundidad desde el primer vértice
    dfs(0, visitado);

    // Si todos los vértices han sido visitados, el grafo es conexo
    return std::all_of(visitado.begin(), visitado.end(), [](bool v) { return v; });
  }
};

// Función principal
int main() {
  // Creamos un grafo con 5 vértices
  grafo g(5);

  // Añadimos las aristas al grafo
  g.añadirArista(0, 1);
  g.añadirArista(0, 2);
  g.añadirArista(1, 2);
  g.añadirArista(1, 3);
  g.añadirArista(2, 4);

  // Comprobamos si el grafo es conexo
  if (g.esConexo()) {
    std::cout << "El grafo es conexo" << std::endl;
  } else {
    std::cout << "El grafo no es conexo" << std::endl;
  }

  return 0;
}
```

Explicación del código:

* **Definición del grafo:**

  - Creamos una estructura `grafo` que representa un grafo.
  - El grafo tiene un número de vértices `V` y una lista de adyacencia `adj`.
  - La lista de adyacencia es un vector de vectores, donde cada vector representa los vértices adyacentes a un vértice dado.

* **Añadir una arista al grafo:**

  - La función `añadirArista` añade una arista al grafo.
  - La arista se añade a la lista de adyacencia de ambos vértices.

* **Búsqueda en profundidad:**

  - La función `dfs` realiza una búsqueda en profundidad del grafo.
  - La función marca los vértices como visitados y recorre los vértices adyacentes.

* **Comprobar si el grafo es conexo:**

  - La función `esConexo` comprueba si el grafo es conexo.
  - La función realiza una búsqueda en profundidad desde el primer vértice y comprueba si todos los vértices han sido visitados.

* **Función principal:**

  - En la función principal, creamos un grafo de ejemplo.
  - Añadimos las aristas al grafo.
  - Comprobamos si el grafo es conexo.

El código anterior es un ejemplo de un grafo conexo. Un grafo conexo es un grafo en el que todos los vértices están conectados entre sí. El código utiliza una búsqueda en profundidad para comprobar si el grafo es conexo. La búsqueda en profundidad se realiza desde el primer vértice y recorre todos los vértices adyacentes. Si todos los vértices han sido visitados, el grafo es conexo.