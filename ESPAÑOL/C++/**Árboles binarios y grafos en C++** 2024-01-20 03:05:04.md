```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

using namespace std;

// Clase que representa un nodo en un árbol binario
class Nodo {
public:
  int valor;
  Nodo* izquierda;
  Nodo* derecha;

  Nodo(int valor) {
    this->valor = valor;
    izquierda = nullptr;
    derecha = nullptr;
  }
};

// Clase que representa un árbol binario
class ÁrbolBinario {
public:
  Nodo* raíz;

  ÁrbolBinario() {
    raíz = nullptr;
  }

  // Inserta un nuevo nodo en el árbol
  void insertar(int valor) {
    Nodo* nuevoNodo = new Nodo(valor);
    if (raíz == nullptr) {
      raíz = nuevoNodo;
    } else {
      insertarRecursivo(nuevoNodo, raíz);
    }
  }

  // Busca un nodo en el árbol
  bool buscar(int valor) {
    return buscarRecursivo(valor, raíz);
  }

  // Elimina un nodo del árbol
  void eliminar(int valor) {
    eliminarRecursivo(valor, raíz);
  }

  // Imprime el árbol en orden ascendente
  void imprimirEnOrden() {
    imprimirEnOrdenRecursivo(raíz);
  }

  // Imprime el árbol en orden descendente
  void imprimirEnOrdenDescendente() {
    imprimirEnOrdenDescendenteRecursivo(raíz);
  }

  // Imprime el árbol en preorden
  void imprimirEnPreorden() {
    imprimirEnPreordenRecursivo(raíz);
  }

  // Imprime el árbol en postorden
  void imprimirEnPostorden() {
    imprimirEnPostordenRecursivo(raíz);
  }

private:
  // Inserta un nuevo nodo en el árbol de forma recursiva
  void insertarRecursivo(Nodo* nuevoNodo, Nodo* nodoActual) {
    if (nuevoNodo->valor < nodoActual->valor) {
      if (nodoActual->izquierda == nullptr) {
        nodoActual->izquierda = nuevoNodo;
      } else {
        insertarRecursivo(nuevoNodo, nodoActual->izquierda);
      }
    } else {
      if (nodoActual->derecha == nullptr) {
        nodoActual->derecha = nuevoNodo;
      } else {
        insertarRecursivo(nuevoNodo, nodoActual->derecha);
      }
    }
  }

  // Busca un nodo en el árbol de forma recursiva
  bool buscarRecursivo(int valor, Nodo* nodoActual) {
    if (nodoActual == nullptr) {
      return false;
    }

    if (nodoActual->valor == valor) {
      return true;
    }

    if (valor < nodoActual->valor) {
      return buscarRecursivo(valor, nodoActual->izquierda);
    } else {
      return buscarRecursivo(valor, nodoActual->derecha);
    }
  }

  // Elimina un nodo del árbol de forma recursiva
  void eliminarRecursivo(int valor, Nodo* nodoActual) {
    if (nodoActual == nullptr) {
      return;
    }

    if (valor < nodoActual->valor) {
      eliminarRecursivo(valor, nodoActual->izquierda);
    } else if (valor > nodoActual->valor) {
      eliminarRecursivo(valor, nodoActual->derecha);
    } else {
      if (nodoActual->izquierda == nullptr && nodoActual->derecha == nullptr) {
        // Caso 1: nodo hoja
        if (nodoActual == raíz) {
          raíz = nullptr;
        } else if (nodoActual->valor < nodoActual->p->valor) {
          nodoActual->p->izquierda = nullptr;
        } else {
          nodoActual->p->derecha = nullptr;
        }
        delete nodoActual;
      } else if (nodoActual->izquierda == nullptr) {
        // Caso 2: nodo con un hijo derecho
        if (nodoActual == raíz) {
          raíz = nodoActual->derecha;
        } else if (nodoActual->valor < nodoActual->p->valor) {
          nodoActual->p->izquierda = nodoActual->derecha;
        } else {
          nodoActual->p->derecha = nodoActual->derecha;
        }
        delete nodoActual;
      } else if (nodoActual->derecha == nullptr) {
        // Caso 3: nodo con un hijo izquierdo
        if (nodoActual == raíz) {
          raíz = nodoActual->izquierda;
        } else if (nodoActual->valor < nodoActual->p->valor) {
          nodoActual->p->izquierda = nodoActual->izquierda;
        } else {
          nodoActual->p->derecha = nodoActual->izquierda;
        }
        delete nodoActual;
      } else {
        // Caso 4: nodo con dos hijos
        Nodo* sucesor = buscarSucesor(nodoActual);
        nodoActual->valor = sucesor->valor;
        eliminarRecursivo(sucesor->valor, nodoActual->derecha);
      }
    }
  }

  // Imprime el árbol en orden ascendente de forma recursiva
  void imprimirEnOrdenRecursivo(Nodo* nodoActual) {
    if (nodoActual == nullptr) {
      return;
    }

    imprimirEnOrdenRecursivo(nodoActual->izquierda);
    cout << nodoActual->valor << " ";
    imprimirEnOrdenRecursivo(nodoActual->derecha);
  }

  // Imprime el árbol en orden descendente de forma recursiva
  void imprimirEnOrdenDescendenteRecursivo(Nodo* nodoActual) {
    if (nodoActual == nullptr) {
      return;
    }

    imprimirEnOrdenDescendenteRecursivo(nodoActual->derecha);
    cout << nodoActual->valor << " ";
    imprimirEnOrdenDescendenteRecursivo(nodoActual->izquierda);
  }

  // Imprime el árbol en preorden de forma recursiva
  void imprimirEnPreordenRecursivo(Nodo* nodoActual) {
    if (nodoActual == nullptr) {
      return;
    }

    cout << nodoActual->valor << " ";
    imprimirEnPreordenRecursivo(nodoActual->izquierda);
    imprimirEnPreordenRecursivo(nodoActual->derecha);
  }

  // Imprime el árbol en postorden de forma recursiva
  void imprimirEnPostordenRecursivo(Nodo* nodoActual) {
    if (nodoActual == nullptr) {
      return;
    }

    imprimirEnPostordenRecursivo(nodoActual->izquierda);
    imprimirEnPostordenRecursivo(nodoActual->derecha);
    cout << nodoActual->valor << " ";
  }

  // Busca el sucesor de un nodo en el árbol
  Nodo* buscarSucesor(Nodo* nodoActual) {
    Nodo* sucesor = nodoActual->derecha;
    while (sucesor->izquierda != nullptr) {
      sucesor = sucesor->izquierda;
    }
    return sucesor;
  }
};

// Clase que representa un grafo
class Grafo {
public:
  vector<vector<int>> adyacencia;

  Grafo(int numNodos) {
    adyacencia.resize(numNodos);
  }

  // Agrega una arista al grafo
  void agregarArista(int nodoOrigen, int nodoDestino) {
    adyacencia[nodoOrigen].push_back(nodoDestino);
  }

  // Recorre el grafo en profundidad (DFS)
  void dfs(int nodoActual) {
    cout << nodoActual << " ";

    for (int vecino : adyacencia[nodoActual]) {
      if (!visitado[vecino]) {
        visitado[vecino] = true;
        dfs(vecino);
      }
    }
  }

  // Recorre el grafo en amplitud (BFS)
  void bfs(int nodoActual) {
    queue<int> cola;
    cola.push(nodoActual);
    visitado[nodoActual] = true;

    while (!cola.empty()) {
      int nodoActual = cola.front();
      cola.pop();

      cout << nodoActual << " ";

      for (int vecino : adyacencia[nodoActual]) {
        if (!visitado[vecino]) {
          visitado[vecino] = true;
          cola.push(vecino);
        }
      }
    }
  }

private:
  vector<bool> visit