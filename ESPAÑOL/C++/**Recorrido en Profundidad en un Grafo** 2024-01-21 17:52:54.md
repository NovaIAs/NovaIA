```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// Clase para representar un grafo
class Grafo {
public:
  // Constructor del grafo
  Grafo(int num_nodos) {
    this->num_nodos = num_nodos;
    this->adyacencia = vector<vector<int>>(num_nodos, vector<int>());
  }

  // Método para agregar una arista al grafo
  void agregar_arista(int nodo1, int nodo2) {
    this->adyacencia[nodo1].push_back(nodo2);
    this->adyacencia[nodo2].push_back(nodo1);
  }

  // Método para obtener el número de nodos del grafo
  int obtener_num_nodos() const {
    return this->num_nodos;
  }

  // Método para obtener la lista de adyacencia del grafo
  const vector<vector<int>>& obtener_adyacencia() const {
    return this->adyacencia;
  }

private:
  // Número de nodos del grafo
  int num_nodos;

  // Lista de adyacencia del grafo
  vector<vector<int>> adyacencia;
};

// Clase para representar un recorrido en profundidad en un grafo
class RecorridoEnProfundidad {
public:
  // Constructor del recorrido en profundidad
  RecorridoEnProfundidad(const Grafo& grafo) {
    this->grafo = &grafo;
    this->visitados = vector<bool>(grafo.obtener_num_nodos(), false);
  }

  // Método para realizar el recorrido en profundidad en el grafo
  void recorrer() {
    for (int i = 0; i < this->grafo->obtener_num_nodos(); i++) {
      if (!this->visitados[i]) {
        this->recorrer_recursivo(i);
      }
    }
  }

private:
  // Referencia al grafo sobre el que se realiza el recorrido
  const Grafo* grafo;

  // Vector de visitados para marcar los nodos que ya han sido visitados
  vector<bool> visitados;

  // Método recursivo para realizar el recorrido en profundidad
  void recorrer_recursivo(int nodo) {
    this->visitados[nodo] = true;
    cout << "Visitando el nodo " << nodo << endl;

    // Recorrer los nodos adyacentes al nodo actual
    for (int adyacente : this->grafo->obtener_adyacencia()[nodo]) {
      if (!this->visitados[adyacente]) {
        this->recorrer_recursivo(adyacente);
      }
    }
  }
};

// Función principal
int main() {
  // Crear un grafo con 5 nodos
  Grafo grafo(5);

  // Agregar aristas al grafo
  grafo.agregar_arista(0, 1);
  grafo.agregar_arista(0, 2);
  grafo.agregar_arista(1, 2);
  grafo.agregar_arista(1, 3);
  grafo.agregar_arista(2, 3);
  grafo.agregar_arista(2, 4);
  grafo.agregar_arista(3, 4);

  // Crear un recorrido en profundidad en el grafo
  RecorridoEnProfundidad recorrido(grafo);

  // Realizar el recorrido en profundidad
  recorrido.recorrer();

  return 0;
}
```

Explicación del código:

* El código crea un grafo con 5 nodos y agrega aristas al grafo para conectar los nodos.

* El código crea un recorrido en profundidad en el grafo y lo realiza.

* El recorrido en profundidad visita todos los nodos del grafo, empezando por el nodo 0, y luego recorre todos los nodos adyacentes al nodo actual, hasta que todos los nodos hayan sido visitados.

* El código imprime el nodo actual que está siendo visitado en cada paso del recorrido en profundidad.