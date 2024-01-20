```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

using namespace std;

// Definición de una clase para representar un grafo
class Grafo {
public:
    // Constructor
    Grafo(int num_nodos) : num_nodos(num_nodos) {
        // Inicializar la lista de adyacencia
        lista_adyacencia.resize(num_nodos);
    }

    // Agregar una arista al grafo
    void agregar_arista(int nodo1, int nodo2, int peso) {
        // Verificar que los nodos sean válidos
        if (nodo1 < 0 || nodo1 >= num_nodos || nodo2 < 0 || nodo2 >= num_nodos) {
            cout << "Error: nodos inválidos" << endl;
            return;
        }

        // Agregar la arista a la lista de adyacencia
        lista_adyacencia[nodo1].push_back({nodo2, peso});
        lista_adyacencia[nodo2].push_back({nodo1, peso});
    }

    // Realizar una búsqueda en profundidad (DFS) a partir de un nodo inicial
    void dfs(int nodo_inicial) {
        // Marcar el nodo inicial como visitado
        visitados[nodo_inicial] = true;

        // Recorrer todos los nodos adyacentes al nodo inicial
        for (auto& arista : lista_adyacencia[nodo_inicial]) {
            // Si el nodo adyacente no ha sido visitado, realizar una DFS recursiva desde ese nodo
            if (!visitados[arista.nodo]) {
                dfs(arista.nodo);
            }
        }
    }

    // Realizar una búsqueda en amplitud (BFS) a partir de un nodo inicial
    void bfs(int nodo_inicial) {
        // Crear una cola para almacenar los nodos que aún no han sido visitados
        queue<int> cola;

        // Agregar el nodo inicial a la cola
        cola.push(nodo_inicial);

        // Marcar el nodo inicial como visitado
        visitados[nodo_inicial] = true;

        // Mientras la cola no esté vacía, seguir visitando nodos
        while (!cola.empty()) {
            // Quitar el primer nodo de la cola
            int nodo_actual = cola.front();
            cola.pop();

            // Recorrer todos los nodos adyacentes al nodo actual
            for (auto& arista : lista_adyacencia[nodo_actual]) {
                // Si el nodo adyacente no ha sido visitado, agregarlo a la cola y marcarlo como visitado
                if (!visitados[arista.nodo]) {
                    cola.push(arista.nodo);
                    visitados[arista.nodo] = true;
                }
            }
        }
    }

    // Encontrar el camino más corto entre dos nodos mediante el algoritmo de Dijkstra
    vector<int> dijkstra(int nodo_inicial, int nodo_final) {
        // Inicializar las distancias de todos los nodos al nodo inicial
        vector<int> distancias(num_nodos, INT_MAX);
        distancias[nodo_inicial] = 0;

        // Crear un conjunto para almacenar los nodos que aún no han sido procesados
        set<int> nodos_no_procesados;
        for (int i = 0; i < num_nodos; i++) {
            nodos_no_procesados.insert(i);
        }

        // Mientras el conjunto de nodos no procesados no esté vacío, seguir procesando nodos
        while (!nodos_no_procesados.empty()) {
            // Encontrar el nodo con la distancia más corta al nodo inicial
            int nodo_actual = *nodos_no_procesados.begin();
            for (int nodo : nodos_no_procesados) {
                if (distancias[nodo] < distancias[nodo_actual]) {
                    nodo_actual = nodo;
                }
            }

            // Quitar el nodo actual del conjunto de nodos no procesados
            nodos_no_procesados.erase(nodo_actual);

            // Recorrer todos los nodos adyacentes al nodo actual
            for (auto& arista : lista_adyacencia[nodo_actual]) {
                // Si el nodo adyacente no ha sido procesado, actualizar su distancia al nodo inicial
                if (nodos_no_procesados.find(arista.nodo) != nodos_no_procesados.end()) {
                    int nueva_distancia = distancias[nodo_actual] + arista.peso;
                    if (nueva_distancia < distancias[arista.nodo]) {
                        distancias[arista.nodo] = nueva_distancia;
                    }
                }
            }
        }

        // Devolver el camino más corto entre el nodo inicial y el nodo final
        vector<int> camino_mas_corto;
        if (distancias[nodo_final] != INT_MAX) {
            camino_mas_corto.push_back(nodo_final);
            int nodo_anterior = nodo_final;
            while (nodo_anterior != nodo_inicial) {
                for (auto& arista : lista_adyacencia[nodo_anterior