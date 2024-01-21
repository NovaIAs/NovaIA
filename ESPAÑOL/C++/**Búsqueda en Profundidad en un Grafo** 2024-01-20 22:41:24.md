```c++
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

class Grafo {
public:
  Grafo(int num_nodos) {
    _num_nodos = num_nodos;
    _adyacencia.resize(_num_nodos);
  }

  void agregar_arista(int nodo1, int nodo2) {
    _adyacencia[nodo1].push_back(nodo2);
    _adyacencia[nodo2].push_back(nodo1);
  }

  vector<vector<int>> obtener_adyacencia() {
    return _adyacencia;
  }

private:
  int _num_nodos;
  vector<vector<int>> _adyacencia;
};

class BusquedaEnProfundidad {
public:
  BusquedaEnProfundidad(Grafo& grafo) {
    _grafo = &grafo;
    _visitados.resize(_grafo->obtener_adyacencia().size(), false);
  }

  void buscar(int nodo_inicial) {
    _visitados[nodo_inicial] = true;
    cout << "Nodo " << nodo_inicial << " visitado\n";

    for (int vecino : _grafo->obtener_adyacencia()[nodo_inicial]) {
      if (!_visitados[vecino]) {
        buscar(vecino);
      }
    }
  }

private:
  Grafo* _grafo;
  vector<bool> _visitados;
};

int main() {
  Grafo grafo(5);
  grafo.agregar_arista(0, 1);
  grafo.agregar_arista(0, 2);
  grafo.agregar_arista(1, 3);
  grafo.agregar_arista(1, 4);
  grafo.agregar_arista(2, 4);

  BusquedaEnProfundidad busqueda_en_profundidad(grafo);
  busqueda_en_profundidad.buscar(0);

  return 0;
}
```

Explicación del código:

1. **Clase Grafo**: Esta clase representa un grafo no dirigido. Tiene un constructor que toma el número de nodos en el grafo como argumento y crea una lista de adyacencia para cada nodo. La lista de adyacencia es una lista de los nodos a los que está conectado cada nodo.

2. **Método agregar_arista**: Este método de la clase Grafo se utiliza para agregar una arista entre dos nodos en el grafo.

3. **Método obtener_adyacencia**: Este método de la clase Grafo devuelve la lista de adyacencia del grafo.

4. **Clase BusquedaEnProfundidad**: Esta clase implementa la búsqueda en profundidad en un grafo. Tiene un constructor que toma una referencia a un objeto de la clase Grafo como argumento y crea un vector de booleanos para mantener un registro de los nodos visitados.

5. **Método buscar**: Este método de la clase BusquedaEnProfundidad realiza una búsqueda en profundidad en el grafo a partir de un nodo inicial. Marca los nodos visitados y los imprime en la consola.

6. **Función main**: Esta función crea un objeto de la clase Grafo y agrega algunas aristas para crear un grafo. Luego, crea un objeto de la clase BusquedaEnProfundidad y llama al método buscar para realizar una búsqueda en profundidad en el grafo a partir del nodo 0.