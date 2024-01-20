```c++
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Clase para representar un vértice en un grafo
class Vertice {
public:
    // Constructor del vértice
    Vertice(int id) {
        this->id = id;
    }

    // Devuelve el identificador del vértice
    int getId() {
        return id;
    }

private:
    // Identificador del vértice
    int id;
};

// Clase para representar un borde en un grafo
class Borde {
public:
    // Constructor del borde
    Borde(Vertice* origen, Vertice* destino, int peso) {
        this->origen = origen;
        this->destino = destino;
        this->peso = peso;
    }

    // Devuelve el vértice de origen del borde
    Vertice* getOrigen() {
        return origen;
    }

    // Devuelve el vértice de destino del borde
    Vertice* getDestino() {
        return destino;
    }

    // Devuelve el peso del borde
    int getPeso() {
        return peso;
    }

private:
    // Vértice de origen del borde
    Vertice* origen;

    // Vértice de destino del borde
    Vertice* destino;

    // Peso del borde
    int peso;
};

// Clase para representar un grafo
class Grafo {
public:
    // Constructor del grafo
    Grafo() {
        // Inicializamos la lista de vértices y la lista de bordes
        vertices = vector<Vertice*>();
        bordes = vector<Borde*>();
    }

    // Añade un vértice al grafo
    void añadirVertice(Vertice* vertice) {
        // Añadimos el vértice a la lista de vértices
        vertices.push_back(vertice);
    }

    // Añade un borde al grafo
    void añadirBorde(Borde* borde) {
        // Añadimos el borde a la lista de bordes
        bordes.push_back(borde);
    }

    // Devuelve la lista de vértices del grafo
    vector<Vertice*> getVertices() {
        return vertices;
    }

    // Devuelve la lista de bordes del grafo
    vector<Borde*> getBord