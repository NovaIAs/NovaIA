```c++
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

// Clase para representar los nodos del grafo.
class Nodo {
public:
    int valor;
    vector<Nodo*> vecinos;

    Nodo(int valor) : valor(valor) {}

    void addVecino(Nodo* vecino) {
        vecinos.push_back(vecino);
    }
};

// Clase para representar el grafo.
class Grafo {
public:
    map<int, Nodo*> nodos;

    void addNodo(int valor) {
        nodos[valor] = new Nodo(valor);
    }

    void addArco(int origen, int destino) {
        nodos[origen]->addVecino(nodos[destino]);
    }

    // Función para realizar la búsqueda en anchura del grafo.
    set<Nodo*> busquedaAnchura(int origen) {
        set<Nodo*> visitados;
        queue<Nodo*> cola;

        cola.push(nodos[origen]);
        visitados.insert(nodos[origen]);

        while (!cola.empty()) {
            Nodo* nodoActual = cola.front();
            cola.pop();

            for (Nodo* vecino : nodoActual->vecinos) {
                if (visitados.find(vecino) == visitados.end()) {
                    cola.push(vecino);
                    visitados.insert(vecino);
                }
            }
        }

        return visitados;
    }

    // Función para realizar la búsqueda en profundidad del grafo.
    set<Nodo*> busquedaProfundidad(int origen) {
        set<Nodo*> visitados;
        stack<Nodo*> pila;

        pila.push(nodos[origen]);
        visitados.insert(nodos[origen]);

        while (!pila.empty()) {
            Nodo* nodoActual = pila.top();
            pila.pop();

            for (Nodo* vecino : nodoActual->vecinos) {
                if (visitados.find(vecino) == visitados.end()) {
                    pila.push(vecino);
                    visitados.insert(vecino);
                }
            }
        }

        return visitados;
    }

    // Función para encontrar el camino más corto entre dos nodos.
    vector<Nodo*> caminoMasCorto(int origen, int destino) {
        vector<Nodo*> camino;
        map<Nodo*, Nodo*> padre;

        queue<Nodo*> cola;
        cola.push(nodos[origen]);
        visitados.insert(nodos[origen]);

        while (!cola.empty()) {
            Nodo* nodoActual = cola.front();
            cola.pop();

            for (Nodo* vecino : nodoActual->vecinos) {
                if (visitados.find(vecino) == visitados.end()) {
                    cola.push(vecino);
                    visitados.insert(vecino);
                    padre[vecino] = nodoActual;
                }
            }
        }

        if (padre.find(nodos[destino]) != padre.end()) {
            Nodo* nodoActual = nodos[destino];
            while (nodoActual != nodos[origen]) {
                camino.push_back(nodoActual);
                nodoActual = padre[nodoActual];
            }
            camino.push_back(nodos[origen]);
            reverse(camino.begin(), camino.end());
        }

        return camino;
    }
};

// Función para probar el grafo.
int main() {
    Grafo grafo;

    grafo.addNodo(1);
    grafo.addNodo(2);
    grafo.addNodo(3);
    grafo.addNodo(4);
    grafo.addNodo(5);

    grafo.addArco(1, 2);
    grafo.addArco(1, 3);
    grafo.addArco(2, 4);
    grafo.addArco(2, 5);
    grafo.addArco(3, 5);

    // Búsqueda en anchura
    set<Nodo*> visitados = grafo.busquedaAnchura(1);
    cout << "Búsqueda en anchura:" << endl;
    for (Nodo* nodo : visitados) {
        cout << nodo->valor << " ";
    }
    cout << endl;

    // Búsqueda en profundidad
    visitados = grafo.busquedaProfundidad(1);
    cout << "Búsqueda en profundidad:" << endl;
    for (Nodo* nodo : visitados) {
        cout << nodo->valor << " ";
    }
    cout << endl;

    // Camino más corto
    vector<Nodo*> camino = grafo.caminoMasCorto(1, 5);
    cout << "Camino más corto entre 1 y 5:" << endl;
    for (Nodo* nodo : camino) {
        cout << nodo->valor << " ";
    }
    cout << endl;

    return 0;
}
```

Explicación del código:

* La clase `Nodo` representa los nodos del grafo. Cada nodo tiene un valor y una lista de vecinos.
* La clase `Grafo` representa el grafo. Un grafo es una colección de nodos conectados por arcos.
* La función `addNodo()` añade un nodo al grafo.
* La función `addArco()` añade un arco entre dos nodos del grafo.
* La función `busquedaAnchura()` realiza una búsqueda en anchura del grafo a partir de un nodo dado.
* La función `busquedaProfundidad()` realiza una búsqueda en profundidad del grafo a partir de un nodo dado.
* La función `caminoMasCorto()` encuentra el camino más corto entre dos nodos del grafo.
* La función `main()` crea un grafo y lo prueba.