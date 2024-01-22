```c++
#include <iostream>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <algorithm>
#include <functional>

using namespace std;

// Definición de la clase Grafo
class Grafo {
public:
    // Constructor del grafo
    Grafo() { }

    // Añadir un vértice al grafo
    void AgregarVertice(int vertice) {
        vertices_.push_back(vertice);
    }

    // Añadir una arista al grafo
    void AgregarArista(int origen, int destino) {
        aristas_[origen].push_back(destino);
    }

    // Obtener el número de vértices del grafo
    int NumeroVertices() {
        return vertices_.size();
    }

    // Obtener el número de aristas del grafo
    int NumeroAristas() {
        int num_aristas = 0;
        for (auto& aristas : aristas_) {
            num_aristas += aristas.size();
        }
        return num_aristas;
    }

    // Obtener la lista de adyacencia de un vértice
    vector<int> ObtenerAdyacentes(int vertice) {
        return aristas_[vertice];
    }

    // Realizar una búsqueda en profundidad (DFS) desde un vértice inicial
    void DFS(int vertice_inicial) {
        // Marcar todos los vértices como no visitados
        vector<bool> visitados(vertices_.size(), false);

        // Iniciar la búsqueda en profundidad desde el vértice inicial
        DFSRecursivo(vertice_inicial, visitados);
    }

    // Realizar una búsqueda en anchura (BFS) desde un vértice inicial
    void BFS(int vertice_inicial) {
        // Marcar todos los vértices como no visitados
        vector<bool> visitados(vertices_.size(), false);

        // Crear una cola para almacenar los vértices a visitar
        queue<int> cola;

        // Encolar el vértice inicial
        cola.push(vertice_inicial);

        // Mientras haya vértices en la cola
        while (!cola.empty()) {
            // Desencolar el siguiente vértice a visitar
            int vertice_actual = cola.front();
            cola.pop();

            // Marcar el vértice actual como visitado
            visitados[vertice_actual] = true;

            // Encolar los vértices adyacentes al vértice actual que no hayan sido visitados
            for (auto& adyacente : aristas_[vertice_actual]) {
                if (!visitados[adyacente]) {
                    cola.push(adyacente);
                }
            }
        }
    }

    // Determinar si el grafo es conexo
    bool EsConexo() {
        // Realizar una búsqueda en profundidad desde un vértice arbitrario
        DFS(0);

        // Comprobar si todos los vértices han sido visitados
        for (auto& visitado : visitados_) {
            if (!visitado) {
                return false;
            }
        }

        return true;
    }

    // Determinar si el grafo es acíclico
    bool EsAcíclico() {
        // Crear una lista de adyacencia para el grafo transpuesto
        vector<vector<int>> aristas_transpuestas(vertices_.size());

        // Construir el grafo transpuesto
        for (int i = 0; i < vertices_.size(); i++) {
            for (auto& adyacente : aristas_[i]) {
                aristas_transpuestas[adyacente].push_back(i);
            }
        }

        // Realizar una búsqueda en profundidad en el grafo transpuesto
        DFS(0, true);

        // Comprobar si todos los vértices han sido visitados
        for (auto& visitado : visitados_) {
            if (!visitado) {
                return false;
            }
        }

        return true;
    }

    // Obtener el árbol de expansión mínimo del grafo utilizando el algoritmo de Prim
    vector<pair<int, int>> ObtenerArbolExpansionMinimo() {
        // Crear un conjunto para almacenar los vértices que ya están en el árbol de expansión mínimo
        set<int> vertices_en_arbol;

        // Crear un mapa para almacenar las distancias mínimas entre los vértices
        map<int, int> distancias_minimas;

        // Crear un mapa para almacenar los vértices que se conectaron al árbol de expansión mínimo
        map<int, int> vertices_predecesores;

        // Inicializar las distancias mínimas y los vértices predecesores
        for (int i = 0; i < vertices_.size(); i++) {
            distancias_minimas[i] = INT_MAX;
            vertices_predecesores[i] = -1;
        }

        // Establecer la distancia mínima del vértice inicial a sí mismo a 0
        distancias_minimas[0] = 0;

        // Mientras haya vértices que no estén en el árbol de expansión mínimo
        while (vertices_en_arbol.size() < vertices_.size()) {
            // Encontrar el vértice con la distancia mínima que aún no está en el árbol de expansión mínimo
            int vertice_actual = -1;
            int distancia_minima = INT_MAX;
            for (auto& vertice : vertices_) {
                if (!vertices_en_arbol.count(vertice) && distancias_minimas[vertice] < distancia_minima) {
                    vertice_actual = vertice;
                    distancia_minima = distancias_minimas[vertice];
                }
            }

            // Añadir el vértice actual al árbol de expansión mínimo
            vertices_en_arbol.insert(vertice_actual);

            // Actualizar las distancias mínimas y los vértices predecesores de los vértices adyacentes al vértice actual
            for (auto& adyacente : aristas_[vertice_actual]) {
                if (!vertices_en_arbol.count(adyacente)) {
                    int distancia_actual = distancias_minimas[adyacente];
                    int distancia_nueva = distancias_minimas[vertice_actual] + 1;
                    if (distancia_nueva < distancia_actual) {
                        distancias_minimas[adyacente] = distancia_nueva;
                        vertices_predecesores[adyacente] = vertice_actual;
                    }
                }
            }
        }

        // Construir el árbol de expansión mínimo a partir de los vértices predecesores
        vector<pair<int, int>> arbol_expansion_minimo;
        for (int i = 1; i < vertices_.size(); i++) {
            arbol_expansion_minimo.push_back(make_pair(vertices_predecesores[i], i));
        }

        return arbol_expansion_minimo;
    }

    // Obtener la ruta más corta entre dos vértices utilizando el algoritmo de Dijkstra
    vector<int> ObtenerRutaMasCorta(int origen, int destino) {
        // Crear un mapa para almacenar las distancias mínimas entre los vértices
        map<int, int> distancias_minimas;

        // Crear un mapa para almacenar los vértices que se conectaron al vértice de origen
        map<int, int> vertices_predecesores;

        // Inicializar las distancias mínimas y los vértices predecesores
        for (int i = 0; i < vertices_.size(); i++) {
            distancias_minimas[i] = INT_MAX;
            vertices_predecesores[i] = -1;
        }

        // Establecer la distancia mínima del vértice