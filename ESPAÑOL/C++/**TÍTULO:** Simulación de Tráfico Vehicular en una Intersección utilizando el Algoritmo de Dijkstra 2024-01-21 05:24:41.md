**Simulación de Tránsito Vehicular en una Intersección**

Este código implementa una simulación de tránsito vehicular en una intersección utilizando el algoritmo de Dijkstra para encontrar las rutas más cortas entre los vehículos y sus destinos.

```c++
#include <iostream>
#include <vector>
#include <queue>
#include <map>

using namespace std;

// Clase que representa a un vehículo
class Vehiculo {
public:
    Vehiculo(int id, int origen, int destino) : id(id), origen(origen), destino(destino) {}

    int id;
    int origen;
    int destino;
};

// Clase que representa una intersección
class Interseccion {
public:
    Interseccion(int id) : id(id) {}

    int id;
    vector<pair<Interseccion*, int>> vecinos;
};

// Función que implementa el algoritmo de Dijkstra para encontrar las rutas más cortas entre los vehículos y sus destinos
map<int, vector<Interseccion*>> dijkstra(Interseccion* interseccionInicial, vector<Vehiculo>& vehiculos) {
    // Inicializamos la distancia de la intersección inicial a sí misma a 0
    map<int, int> distancias;
    distancias[interseccionInicial->id] = 0;

    // Inicializamos la ruta de la intersección inicial a sí misma a sí misma
    map<int, vector<Interseccion*>> rutas;
    rutas[interseccionInicial->id] = {interseccionInicial};

    // Creamos una cola de prioridad para almacenar las intersecciones que aún no hemos visitado
    priority_queue<pair<int, Interseccion*>, vector<pair<int, Interseccion*>>, greater<pair<int, Interseccion*>>> cola;
    cola.push({0, interseccionInicial});

    // Mientras haya intersecciones en la cola
    while (!cola.empty()) {
        // Obtenemos la intersección con la distancia más corta
        pair<int, Interseccion*> interseccionActual = cola.top();
        cola.pop();

        // Si la distancia de la intersección actual es mayor que la distancia mínima ya encontrada, la ignoramos
        if (interseccionActual.first > distancias[interseccionActual.second->id]) {
            continue;
        }

        // Recorremos los vecinos de la intersección actual
        for (auto vecino : interseccionActual.second->vecinos) {
            // Calculamos la distancia del vecino a la intersección actual
            int distanciaVecino = interseccionActual.first + vecino.second;

            // Si la distancia del vecino es menor que la distancia actual, actualizamos la distancia y la ruta
            if (distanciaVecino < distancias[vecino.first->id]) {
                distancias[vecino.first->id] = distanciaVecino;
                rutas[vecino.first->id] = rutas[interseccionActual.second->id];
                rutas[vecino.first->id].push_back(vecino.first);

                // Añadimos el vecino a la cola de prioridad
                cola.push({distanciaVecino, vecino.first});
            }
        }
    }

    // Devolvemos las rutas de los vehículos
    map<int, vector<Interseccion*>> rutasVehiculos;
    for (auto vehiculo : vehiculos) {
        rutasVehiculos[vehiculo.id] = rutas[vehiculo.destino];
    }

    return rutasVehiculos;
}

// Función principal
int main() {
    // Creamos las intersecciones
    Interseccion interseccion1(1);
    Interseccion interseccion2(2);
    Interseccion interseccion3(3);
    Interseccion interseccion4(4);

    // Conectamos las intersecciones
    interseccion1.vecinos.push_back({&interseccion2, 10});
    interseccion1.vecinos.push_back({&interseccion3, 5});
    interseccion2.vecinos.push_back({&interseccion4, 15});
    interseccion3.vecinos.push_back({&interseccion4, 20});

    // Creamos los vehículos
    Vehiculo vehiculo1(1, 1, 4);
    Vehiculo vehiculo2(2, 2, 3);
    Vehiculo vehiculo3(3, 3, 1);

    // Obtenemos las rutas de los vehículos
    map<int, vector<Interseccion*>> rutasVehiculos = dijkstra(&interseccion1, {vehiculo1, vehiculo2, vehiculo3});

    // Imprimimos las rutas de los vehículos
    for (auto ruta : rutasVehiculos) {
        cout << "Ruta del vehículo " << ruta.first << ": ";
        for (auto interseccion : ruta.second) {
            cout << interseccion->id << " ";
        }
        cout << endl;
    }

    return 0;
}
```

**Explicación del código:**

1. **Clases Vehiculo e Interseccion:** Estas clases representan los vehículos y las intersecciones, respectivamente.

2. **Función dijkstra:** Esta función implementa el algoritmo de Dijkstra para encontrar las rutas más cortas entre los vehículos y sus destinos.

3. **Función main:** Esta función crea las intersecciones, los vehículos y llama a la función dijkstra para obtener las rutas de los vehículos.

4. **Algoritmo de Dijkstra:** El algoritmo de Dijkstra funciona de la siguiente manera:

   - Inicializamos la distancia de la intersección inicial a sí misma a 0.
   - Inicializamos la ruta de la intersección inicial a sí misma a sí misma.
   - Creamos una cola de prioridad para almacenar las intersecciones que aún no hemos visitado.
   - Mientras haya intersecciones en la cola:
     - Obtenemos la intersección con la distancia más corta.
     - Recorremos los vecinos de la intersección actual.
     - Calculamos la distancia del vecino a la intersección actual.
     - Si la distancia del vecino es menor que la distancia actual, actualizamos la distancia y la ruta.
     - Añadimos el vecino a la cola de prioridad.

5. **Impresión de las rutas:** Una vez que hemos obtenido las rutas de los vehículos, las imprimimos en la consola.