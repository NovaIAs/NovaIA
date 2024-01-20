```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// Definición de la clase Grafo
class Grafo {
public:
    // Constructor
    Grafo(int num_nodos) {
        this->num_nodos = num_nodos;
        this->lista_adyacencia.resize(num_nodos);
    }

    // Añadir una arista al grafo
    void añadir_arista(int nodo1, int nodo2, int peso) {
        this->lista_adyacencia[nodo1].push_back(make_pair(nodo2, peso));
    }

    // Realizar una búsqueda en profundidad (DFS)
    void dfs(int nodo_inicial) {
        // Inicializar el vector de visitados
        vector<bool> visitados(this->num_nodos, false);

        // Pila para el recorrido DFS
        stack<int> pila;

        // Marcar el nodo inicial como visitado y añadirlo a la pila
        visitados[nodo_inicial] = true;
        pila.push(nodo_inicial);

        // Mientras la pila no esté vacía, seguir explorando
        while (!pila.empty()) {
            // Obtener el nodo actual de la pila
            int nodo_actual = pila.top();
            pila.pop();

            // Visitar el nodo actual
            cout << nodo_actual << " ";

            // Explorar los nodos adyacentes al nodo actual
            for (auto vecino : this->lista_adyacencia[nodo_actual]) {
                int nodo_vecino = vecino.first;
                int peso = vecino.second;

                // Si el nodo vecino no ha sido visitado, marcarlo como visitado y añadirlo a la pila
                if (!visitados[nodo_vecino]) {
                    visitados[nodo_vecino] = true;
                    pila.push(nodo_vecino);
                }
            }
        }
    }

    // Realizar una búsqueda en anchura (BFS)
    void bfs(int nodo_inicial) {
        // Inicializar el vector de visitados
        vector<bool> visitados(this->num_nodos, false);

        // Cola para el recorrido BFS
        queue<int> cola;

        // Marcar el nodo inicial como visitado y añadirlo a la cola
        visitados[nodo_inicial] = true;
        cola.push(nodo_inicial);

        // Mientras la cola no esté vacía, seguir explorando
        while (!cola.empty()) {
            // Obtener el nodo actual de la cola
            int nodo_actual = cola.front();
            cola.pop();

            // Visitar el nodo actual
            cout << nodo_actual << " ";

            // Explorar los nodos adyacentes al nodo actual
            for (auto vecino : this->lista_adyacencia[nodo_actual]) {
                int nodo_vecino = vecino.first;
                int peso = vecino.second;

                // Si el nodo vecino no ha sido visitado, marcarlo como visitado y añadirlo a la cola
                if (!visitados[nodo_vecino]) {
                    visitados[nodo_vecino] = true;
                    cola.push(nodo_vecino);
                }
            }
        }
    }

    // Encontrar el camino más corto entre dos nodos utilizando el algoritmo de Dijkstra
    vector<int> dijkstra(int nodo_inicial, int nodo_final) {
        // Inicializar el vector de distancias
        vector<int> distancias(this->num_nodos, INT_MAX);

        // Inicializar el vector de nodos visitados
        vector<bool> visitados(this->num_nodos, false);

        // Inicializar el nodo inicial con distancia 0
        distancias[nodo_inicial] = 0;

        // Cola de prioridad para el algoritmo de Dijkstra
        priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> cola;

        // Añadir el nodo inicial a la cola de prioridad
        cola.push(make_pair(0, nodo_inicial));

        // Mientras la cola de prioridad no esté vacía, seguir explorando
        while (!cola.empty()) {
            // Obtener el nodo actual de la cola de prioridad
            int nodo_actual = cola.top().second;
            cola.pop();

            // Si el nodo actual es el nodo final, hemos encontrado el camino más corto
            if (nodo_actual == nodo_final) {
                break;
            }

            // Si el nodo actual ya ha sido visitado, continuar
            if (visitados[nodo_actual]) {
                continue;
            }

            // Marcar el nodo actual como visitado
            visitados[nodo_actual] = true;

            // Explorar los nodos adyacentes al nodo actual
            for (auto vecino : this->lista_adyacencia[nodo_actual]) {
                int nodo_vecino = vecino.first;
                int peso = vecino.second;

                // Si el nodo vecino no ha sido visitado y la nueva distancia es menor que la distancia actual, actualizar la distancia y añadir el nodo vecino a la cola de prioridad
                if (!visitados[nodo_vecino] && distancias[nodo_actual] + peso < distancias[nodo_vecino]) {
                    distancias[nodo_vecino] = distancias[nodo_actual] + peso;
                    cola.push(make_pair(distancias[nodo_vecino], nodo_vecino));
                }
            }
        }

        // Si la distancia del nodo final es infinita, no hay camino entre el nodo inicial y el nodo final
        if (distancias[nodo_final] == INT_MAX) {
            return {};
        }

        // Reconstruir el camino más corto
        vector<int> camino;
        int nodo_actual = nodo_final;

        while (nodo_actual != nodo_inicial) {
            camino.push_back(nodo_actual);
            for (auto vecino : this->lista_adyacencia[nodo_actual]) {
                int nodo_vecino = vecino.first;
                int peso = vecino.second;

                if (distancias[nodo_actual] - peso == distancias[nodo_vecino]) {
                    nodo_actual = nodo_vecino;
                    break;
                }
            }
        }

        camino.push_back(nodo_inicial);
        reverse(camino.begin(), camino.end());

        return camino;
    }

    // Encontrar el árbol de expansión mínimo utilizando el algoritmo de Kruskal
    vector<pair<int, pair<int, int>>> kruskal() {
        // Inicializar el conjunto de nodos disjuntos
        DSU dsu(this->num_nodos);

        // Añadir todas las aristas al conjunto de aristas
        vector<pair<int, pair<int, int>>> aristas;
        for (int i = 0; i < this->num_nodos; i++) {
            for (auto vecino : this->lista_adyacencia[i]) {
                int nodo_vecino = vecino.first;
                int peso = vecino.second;

                aristas.push_back(make_pair(peso, make_pair(i, nodo_vecino)));
            }
        }

        // Ordenar las aristas por peso
        sort(aristas.begin(), aristas.end());

        // Inicializar el árbol de expansión mínimo
        vector<pair<int, pair<int, int>>> mst;

        // Iterar sobre las aristas ordenadas
        for (auto arista : aristas) {
            int peso = arista.first;
            int nodo1 = arista.second.first;
            int nodo2 = arista.second.second;

            // Si los dos nodos de la arista actual no están en el mismo conjunto, añadir la arista al árbol de expansión mínimo y unir los dos conjuntos
            if (!dsu.en_mismo_conjunto(nodo1, nodo2)) {
                mst.push_back(arista);
                dsu.union(nodo1, nodo2);
            }
        }

        return mst;
    }

private:
    // Número de nodos del grafo
    int num_nodos;

    // Lista de adyacencia del grafo
    vector<vector<pair<int, int>>> lista_adyacencia;

    // Clase para implementar el conjunto de nodos disjuntos
    class DSU {
    public:
        // Constructor
        DSU(int num_nodos) {
            this->num_nodos = num_nodos;
            this->padres.resize(num_nodos);
            this->ranks.resize(num_nodos);
            for (int i = 0; i < num_nodos; i++) {
                this->padres[i] = i;
                this->ranks[i] = 0;
            }
        }

        // Encontrar el padre del nodo especificado
        int find(int nodo) {
            if (this->padres[nodo] != nodo) {
                this->padres[nodo] = find(this->padres[nodo]);
            }
            return this->padres[nodo];
        }

        // Unir dos conjuntos disjuntos
        void union(int nodo1, int nodo2) {
            int padre1 = find(nodo1);
            int padre2 = find(nodo2);

            if (padre1 == padre2) {
                return;
            }

            if (this->ranks[padre1] > this->ranks[padre2]) {
                this->padres[padre2] = padre1;
            } else if (this->ranks[padre1] < this->ranks[padre2]) {
                this->padres[padre1] = padre2;
            } else {
                this->padres[padre2] = padre1;
                this->ranks[padre1]++;
            }
        }

        // Comprobar si dos nodos están en el mismo conjunto
        bool en_mismo_conjunto(int nodo1, int nodo2) {
            return find(nodo1) == find(nodo2);
        }

    private:
        // Número de nodos del conjunto disjunto
        int num_nodos;

        // Vector de padres de los nodos
        vector<int> padres;

        // Vector de rangos de los nodos
        vector<int> ranks;
    };
};

// Función principal
int main() {
    // Crear un grafo con 9 nodos y 14 aristas
    Grafo grafo(9);
    grafo.agregar_arista(0, 1, 4);
    grafo.agregar_arista(0, 6, 7);
    grafo.agregar_arista(1, 2, 9);
    grafo.agregar_arista(1, 6, 14);
    grafo.agregar_arista(2, 3, 2);
    grafo.agregar_arista(2, 5, 1);
    grafo.agregar_arista(3, 4, 6);
    grafo.agregar_arista(4, 5, 10);
    grafo.agregar_arista(5, 8, 5);
    grafo.agregar_arista(6, 7, 2);
    grafo.agregar_arista(6, 8, 1);
    grafo.agregar_arista(7, 8, 3);

    // Mostrar el grafo
    cout << "Lista de adyacencia del grafo:" << endl;
    for (int i = 0; i < grafo.num_nodos; i++) {
        cout << i << ": ";
        for (auto vecino : grafo.lista_adyacencia[i]) {
            cout << "(" << vecino.first << ", " << vecino.second << ") ";
        }
        cout << endl;
    }

    // Realizar un recorrido en profundidad del grafo
    cout << "Recorrido en profundidad del grafo:" << endl;
    grafo.dfs(0);

    // Realizar un recorrido en anchura del grafo
    cout << "Recorrido en anchura del grafo:" << endl;
    grafo.bfs(0);

    // Encontrar el camino más corto entre dos nodos del grafo
    int nodo1 = 0;
    int nodo2 = 8;
    vector<int> camino = grafo.dijkstra(nodo1, nodo2);
    if (camino.empty()) {
        cout << "No hay camino entre los nodos " << nodo1 << " y " << nodo2 << endl;
    } else {
        cout << "El camino más corto entre los nodos " << nodo1 << " y " << nodo2 << " es:" << endl;
        for (auto nodo : camino) {
            cout << nodo << " ";
        }
        cout << endl;
    }

    // Encontrar el árbol de expansión mínimo del grafo
    vector<pair<int, pair<int, int>>> mst = grafo.kruskal();
    cout << "El árbol de expansión mínimo del grafo es:" << endl;
    for (auto arista : mst) {
        int peso = arista.first;
        int nodo1 = arista.second.first;
        int nodo2 = arista.second.second;
        cout << "(" << nodo1 << ", " << nodo2 << ", " << peso << ") ";
    }
    cout << endl;

    return 0;
}