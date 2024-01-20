```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

using namespace std;

// Definición de la estructura de datos "Grafo".
struct Grafo {
  // Representación del grafo como una lista de adyacencia.
  vector<vector<int>> adyacencia;
  // Función para añadir un nodo al grafo.
  void anadirNodo() {
    adyacencia.push_back(vector<int>());
  }
  // Función para añadir un borde entre dos nodos del grafo.
  void anadirBorde(int nodo1, int nodo2) {
    adyacencia[nodo1].push_back(nodo2);
    adyacencia[nodo2].push_back(nodo1);
  }
  // Función para realizar una búsqueda en profundidad (DFS) en el grafo.
  void dfs(int nodoActual, vector<bool>& visitados) {
    visitados[nodoActual] = true;
    for (int vecino : adyacencia[nodoActual]) {
      if (!visitados[vecino]) {
        dfs(vecino, visitados);
      }
    }
  }
  // Función para comprobar si el grafo es conexo.
  bool esConexo() {
    // Inicializar el vector de visitados a false.
    vector<bool> visitados(adyacencia.size(), false);
    // Realizar una búsqueda en profundidad desde el primer nodo.
    dfs(0, visitados);
    // Comprobar si todos los nodos han sido visitados.
    for (bool visitado : visitados) {
      if (!visitado) {
        return false;
      }
    }
    return true;
  }
  // Función para encontrar el número de componentes conexas en el grafo.
  int numComponentesConexas() {
    // Inicializar el vector de visitados a false.
    vector<bool> visitados(adyacencia.size(), false);
    // Inicializar el número de componentes conexas a 0.
    int numComponentes = 0;
    // Recorrer todos los nodos del grafo.
    for (int i = 0; i < adyacencia.size(); i++) {
      // Si el nodo no ha sido visitado, realizar una búsqueda en profundidad desde ese nodo.
      if (!visitados[i]) {
        dfs(i, visitados);
        // Incrementar el número de componentes conexas.
        numComponentes++;
      }
    }
    return numComponentes;
  }
  // Función para encontrar el camino más corto entre dos nodos del grafo utilizando el algoritmo de Dijkstra.
  vector<int> caminoMasCortoDijkstra(int nodoOrigen, int nodoDestino) {
    // Inicializar la distancia mínima de cada nodo al nodo origen.
    vector<int> distancia(adyacencia.size(), INT_MAX);
    // Inicializar el nodo origen con distancia 0.
    distancia[nodoOrigen] = 0;
    // Inicializar el conjunto de nodos pendientes de visitar.
    set<int> pendientes;
    // Añadir el nodo origen al conjunto de pendientes.
    pendientes.insert(nodoOrigen);
    // Mientras haya nodos pendientes de visitar:
    while (!pendientes.empty()) {
      // Obtener el nodo con la distancia mínima al nodo origen.
      int nodoActual = *pendientes.begin();
      // Eliminar el nodo actual del conjunto de pendientes.
      pendientes.erase(pendientes.begin());
      // Recorrer los vecinos del nodo actual.
      for (int vecino : adyacencia[nodoActual]) {
        // Calcular la nueva distancia al nodo vecino.
        int nuevaDistancia = distancia[nodoActual] + 1;
        // Si la nueva distancia es menor que la distancia actual del nodo vecino:
        if (nuevaDistancia < distancia[vecino]) {
          // Actualizar la distancia del nodo vecino.
          distancia[vecino] = nuevaDistancia;
          // Añadir el nodo vecino al conjunto de pendientes.
          pendientes.insert(vecino);
        }
      }
    }
    // Construir el camino más corto desde el nodo origen al nodo destino.
    vector<int> camino;
    int nodoActual = nodoDestino;
    while (nodoActual != nodoOrigen) {
      camino.insert(camino.begin(), nodoActual);
      // Obtener el nodo anterior en el camino.
      nodoActual = anterior[nodoActual];
    }
    // Añadir el nodo origen al camino.
    camino.insert(camino.begin(), nodoOrigen);
    return camino;
  }
};

// Función para imprimir la representación del grafo como una lista de adyacencia.
void imprimirGrafo(const Grafo& grafo) {
  for (int i = 0; i < grafo.adyacencia.size(); i++) {
    cout << "Nodo " << i << ": ";
    for (int vecino : grafo.adyacencia[i]) {
      cout << vecino << " ";
    }
    cout << endl;
  }
}

int main() {
  // Crear un objeto Grafo.
  Grafo grafo;

  // Añadir nodos al grafo.
  grafo.anadirNodo();
  grafo.anadirNodo();
  grafo.anadirNodo();
  grafo.anadirNodo();
  grafo.anadirNodo();

  // Añadir bordes al grafo.
  grafo.anadirBorde(0, 1);
  grafo.anadirBorde(0, 2);
  grafo.anadirBorde(1, 2);
  grafo.anadirBorde(1, 3);
  grafo.anadirBorde(2, 3);
  grafo.anadirBorde(2, 4);
  grafo.anadirBorde(3, 4);

  // Imprimir la representación del grafo.
  imprimirGrafo(grafo);

  // Comprobar si el grafo es conexo.
  if (grafo.esConexo()) {
    cout << "El grafo es conexo." << endl;
  } else {
    cout << "El grafo no es conexo." << endl;
  }

  // Encontrar el número de componentes conexas en el grafo.
  int numComponentes = grafo.numComponentesConexas();
  cout << "El número de componentes conexas en el grafo es: " << numComponentes << endl;

  // Encontrar el camino más corto entre dos nodos del grafo.
  vector<int> camino = grafo.caminoMasCortoDijkstra(0, 4);
  cout << "El camino más corto entre los nodos 0 y 4 es: ";
  for (int nodo : camino) {
    cout << nodo << " ";
  }
  cout << endl;

  return 0;
}
```

Explicación del código:

* La estructura de datos "Grafo" representa un grafo como una lista de adyacencia.
* La función "anadirNodo()" añade un nodo al grafo.
* La función "anadirBorde()" añade un borde entre dos nodos del grafo.
* La función "dfs()" realiza una búsqueda en profundidad (DFS) en el grafo a partir de un nodo dado.
* La función "esConexo()" comprueba si el grafo es conexo.
* La función "numComponentesConexas()" encuentra el número de componentes conexas en el grafo.
* La función "caminoMasCortoDijkstra()" encuentra el camino más corto entre dos nodos del grafo utilizando el algoritmo de Dijkstra.
* La función "imprimirGrafo()" imprime la representación del grafo como una lista de adyacencia.

En la función "main()", se crea un objeto Grafo y se añaden nodos y bordes al grafo. Luego, se imprimen la representación del grafo y se comprueba si es conexo. También se encuentra el número de componentes conexas y el camino más corto entre dos nodos del grafo.