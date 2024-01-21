```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

class Grafo {
public:
  Grafo(int num_nodos) {
    nodos.resize(num_nodos);
  }

  void agregar_borde(int origen, int destino, int peso) {
    nodos[origen].push_back({destino, peso});
  }

  int dijkstra(int origen, int destino) {
    vector<int> distancias(nodos.size(), INT_MAX);
    distancias[origen] = 0;
    priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> pq;
    pq.push({0, origen});

    while (!pq.empty()) {
      auto [distancia, nodo] = pq.top();
      pq.pop();

      if (nodo == destino) {
        return distancia;
      }

      for (auto &[vecino, peso] : nodos[nodo]) {
        int nueva_distancia = distancia + peso;
        if (nueva_distancia < distancias[vecino]) {
          distancias[vecino] = nueva_distancia;
          pq.push({nueva_distancia, vecino});
        }
      }
    }

    return -1;
  }

private:
  vector<vector<pair<int, int>>> nodos;
};

int main() {
  Grafo grafo(6);
  grafo.agregar_borde(0, 1, 4);
  grafo.agregar_borde(0, 2, 2);
  grafo.agregar_borde(1, 2, 5);
  grafo.agregar_borde(1, 3, 10);
  grafo.agregar_borde(2, 3, 8);
  grafo.agregar_borde(2, 4, 7);
  grafo.agregar_borde(2, 5, 6);
  grafo.agregar_borde(3, 4, 8);
  grafo.agregar_borde(3, 5, 5);
  grafo.agregar_borde(4, 5, 2);

  int distancia_minima = grafo.dijkstra(0, 5);
  cout << "La distancia mínima entre el nodo 0 y el nodo 5 es: " << distancia_minima << endl;

  return 0;
}
```

Este código implementa el algoritmo de Dijkstra para encontrar la ruta más corta entre dos nodos en un grafo ponderado.

El algoritmo de Dijkstra se basa en la idea de mantener una lista de nodos conocidos y una lista de nodos desconocidos. El nodo conocido inicial es el nodo origen, y todos los demás nodos son desconocidos.

En cada paso del algoritmo, se selecciona el nodo conocido con la distancia más corta al nodo origen y se añaden sus vecinos a la lista de nodos conocidos. Se calcula la distancia más corta a cada vecino y se actualiza la lista de distancias más cortas si es necesario.

El algoritmo termina cuando se alcanza el nodo destino o cuando no hay más nodos conocidos.

En este código, la clase Grafo representa el grafo y el método dijkstra implementa el algoritmo de Dijkstra. El método dijkstra toma dos argumentos: el nodo origen y el nodo destino, y devuelve la distancia más corta entre ellos.

El método dijkstra utiliza una cola de prioridad para mantener una lista de nodos conocidos y una lista de nodos desconocidos. La cola de prioridad se ordena por la distancia al nodo origen, de modo que el nodo con la distancia más corta siempre está en la parte superior de la cola.

En cada paso del algoritmo, se selecciona el nodo conocido con la distancia más corta al nodo origen y se añaden sus vecinos a la lista de nodos conocidos. Se calcula la distancia más corta a cada vecino y se actualiza la lista de distancias más cortas si es necesario.

El algoritmo termina cuando se alcanza el nodo destino o cuando no hay más nodos conocidos.

El código principal crea un grafo con 6 nodos y 10 bordes, e invoca al método dijkstra para encontrar la distancia más corta entre el nodo 0 y el nodo 5. El resultado se imprime en la consola.