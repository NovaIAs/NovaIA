```c++
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

class Grafo {
public:
    Grafo(int num_nodos) {
        this->num_nodos = num_nodos;
        this->lista_adyacencia.resize(num_nodos);
    }

    void agregar_arista(int nodo_inicio, int nodo_fin, int peso) {
        this->lista_adyacencia[nodo_inicio].push_back(make_pair(nodo_fin, peso));
    }

    int dijkstra(int nodo_inicio, int nodo_fin) {
        vector<int> distancia(this->num_nodos, INT_MAX);
        distancia[nodo_inicio] = 0;

        priority_queue<pair<int, int>> pq;
        pq.push(make_pair(0, nodo_inicio));

        while (!pq.empty()) {
            int nodo_actual = pq.top().second;
            pq.pop();

            for (auto vecino : this->lista_adyacencia[nodo_actual]) {
                int nodo_vecino = vecino.first;
                int peso_arista = vecino.second;

                if (distancia[nodo_actual] + peso_arista < distancia[nodo_vecino]) {
                    distancia[nodo_vecino] = distancia[nodo_actual] + peso_arista;
                    pq.push(make_pair(-distancia[nodo_vecino], nodo_vecino));
                }
            }
        }

        return distancia[nodo_fin];
    }

private:
    int num_nodos;
    vector<vector<pair<int, int>>> lista_adyacencia;
};

int main() {
    // Crear un grafo con 5 nodos
    Grafo grafo(5);

    // Agregar aristas al grafo
    grafo.agregar_arista(0, 1, 10);
    grafo.agregar_arista(1, 2, 15);
    grafo.agregar_arista(2, 3, 20);
    grafo.agregar_arista(3, 4, 25);
    grafo.agregar_arista(0, 4, 30);

    // Calcular la distancia más corta entre el nodo 0 y el nodo 4 usando el algoritmo de Dijkstra
    int distancia_mas_corta = grafo.dijkstra(0, 4);

    // Imprimir la distancia más corta
    cout << "La distancia más corta entre el nodo 0 y el nodo 4 es: " << distancia_mas_corta << endl;

    return 0;
}
```

Explicación del código:

1. **Clase Grafo:**

   - Esta clase representa un grafo.
   - El constructor de la clase toma el número de nodos en el grafo como argumento.
   - La función `agregar_arista()` se utiliza para agregar una arista al grafo.
   - La función `dijkstra()` implementa el algoritmo de Dijkstra para encontrar la distancia más corta entre dos nodos en el grafo.

2. **Función main():**

   - Esta función es el punto de entrada del programa.
   - Crea un objeto de la clase Grafo con 5 nodos.
   - Agrega aristas al grafo usando la función `agregar_arista()`.
   - Llama a la función `dijkstra()` para calcular la distancia más corta entre el nodo 0 y el nodo 4.
   - Imprime la distancia más corta.

Este código es un ejemplo de cómo implementar el algoritmo de Dijkstra en C++. El algoritmo de Dijkstra es un algoritmo eficiente para encontrar la distancia más corta entre dos nodos en un grafo ponderado. El código está bien organizado y es fácil de entender.