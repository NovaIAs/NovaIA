```c++
// Programa para encontrar el camino más corto entre dos ciudades en un mapa de carreteras.

#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <queue>

using namespace std;

class Grafo {
public:
    Grafo(int num_ciudades) {
        // Inicializar el grafo con el número especificado de ciudades.
        ciudades.resize(num_ciudades);
    }

    void agregar_carretera(int ciudad1, int ciudad2, int distancia) {
        // Agregar una carretera entre dos ciudades con la distancia especificada.
        ciudades[ciudad1].vecinos.push_back(make_pair(ciudad2, distancia));
        ciudades[ciudad2].vecinos.push_back(make_pair(ciudad1, distancia));
    }

    int encontrar_camino_mas_corto(int ciudad_inicio, int ciudad_destino) {
        // Inicializar las distancias de todas las ciudades al infinito.
        vector<int> distancias(ciudades.size(), numeric_limits<int>::max());

        // Establecer la distancia de la ciudad inicial a 0.
        distancias[ciudad_inicio] = 0;

        // Crear una cola de prioridad para almacenar las ciudades que aún necesitan ser visitadas.
        priority_queue<pair<int, int>> cola;

        // Agregar la ciudad inicial a la cola de prioridad.
        cola.push(make_pair(0, ciudad_inicio));

        // Mientras la cola de prioridad no esté vacía, procesar la próxima ciudad.
        while (!cola.empty()) {
            // Obtener la ciudad con la distancia más corta desde la ciudad inicial.
            int ciudad_actual = cola.top().second;
            int distancia_actual = -cola.top().first;

            // Eliminar la ciudad de la cola de prioridad.
            cola.pop();

            // Revisar si la ciudad actual es el destino.
            if (ciudad_actual == ciudad_destino) {
                return distancia_actual;
            }

            // Revisar los vecinos de la ciudad actual.
            for (auto vecino : ciudades[ciudad_actual].vecinos) {
                // Obtener la ciudad vecina y la distancia a la ciudad vecina.
                int ciudad_vecina = vecino.first;
                int distancia_vecina = vecino.second;

                // Calcular la distancia total a la ciudad vecina.
                int distancia_total = distancia_actual + distancia_vecina;

                // Si la distancia total es más corta que la distancia actual a la ciudad vecina, actualizar la distancia y agregar la ciudad vecina a la cola de prioridad.
                if (distancia_total < distancias[ciudad_vecina]) {
                    distancias[ciudad_vecina] = distancia_total;
                    cola.push(make_pair(-distancia_total, ciudad_vecina));
                }
            }
        }

        // Si no se encontró ningún camino, devolver -1.
        return -1;
    }

private:
    struct Ciudad {
        vector<pair<int, int>> vecinos;
    };

    vector<Ciudad> ciudades;
};

int main() {
    // Crear un grafo con 9 ciudades.
    Grafo grafo(9);

    // Agregar las carreteras entre las ciudades.
    grafo.agregar_carretera(0, 1, 4);
    grafo.agregar_carretera(0, 6, 7);
    grafo.agregar_carretera(1, 2, 9);
    grafo.agregar_carretera(1, 6, 14);
    grafo.agregar_carretera(2, 3, 2);
    grafo.agregar_carretera(2, 8, 1);
    grafo.agregar_carretera(3, 4, 10);
    grafo.agregar_carretera(3, 6, 15);
    grafo.agregar_carretera(4, 5, 6);
    grafo.agregar_carretera(4, 7, 1);
    grafo.agregar_carretera(5, 8, 11);
    grafo.agregar_carretera(6, 7, 2);
    grafo.agregar_carretera(7, 8, 9);

    // Encontrar el camino más corto entre la ciudad 0 y la ciudad 8.
    int distancia = grafo.encontrar_camino_mas_corto(0, 8);

    // Imprimir la distancia más corta.
    cout << "La distancia más corta entre la ciudad 0 y la ciudad 8 es: " << distancia << endl;

    return 0;
}
```

Explicación del código:

* La clase `Grafo` representa el mapa de carreteras. Tiene un vector de `Ciudad`es, donde cada `Ciudad` tiene una lista de `vecinos`, que son otras `Ciudad`es con las que está conectada.
* La función `agregar_carretera()` agrega una carretera entre dos `Ciudad`es con la distancia especificada.
* La función `encontrar_camino_mas_corto()` encuentra el camino más corto entre dos `Ciudad`es usando el algoritmo de Dijkstra. El algoritmo funciona manteniendo una cola de prioridad de `Ciudad`es que aún necesitan ser visitadas, y procesando la próxima `Ciudad` con la distancia más corta desde la `Ciudad` inicial.
* La función `main()` crea un `Grafo` con 9 `Ciudad`es y agrega las carreteras entre ellas. Luego, llama a la función `encontrar_camino_mas_corto()` para encontrar el camino más corto entre la ciudad 0 y la ciudad 8, e imprime la distancia más corta.