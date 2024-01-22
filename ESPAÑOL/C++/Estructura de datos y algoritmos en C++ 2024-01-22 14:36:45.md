```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

// Clase para representar un grafo
class Grafo {
public:
  // Constructor
  Grafo(int num_nodos) {
    num_nodos_ = num_nodos;
    adyacencia_ = vector<vector<int>>(num_nodos_);
  }

  // Añadir un arco
  void agregar_arco(int u, int v) {
    adyacencia_[u].push_back(v);
  }

  // Realizar un recorrido en profundidad (DFS)
  void dfs(int nodo, vector<bool>& visitados) {
    visitados[nodo] = true;
    cout << nodo << " ";

    for (int vecino : adyacencia_[nodo]) {
      if (!visitados[vecino]) {
        dfs(vecino, visitados);
      }
    }
  }

  // Realizar un recorrido en anchura (BFS)
  void bfs(int nodo) {
    queue<int> cola;
    vector<bool> visitados(num_nodos_, false);

    cola.push(nodo);
    visitados[nodo] = true;

    while (!cola.empty()) {
      int nodo_actual = cola.front();
      cola.pop();

      cout << nodo_actual << " ";

      for (int vecino : adyacencia_[nodo_actual]) {
        if (!visitados[vecino]) {
          cola.push(vecino);
          visitados[vecino] = true;
        }
      }
    }
  }

  // Encontrar el camino más corto entre dos nodos
  int dijkstra(int nodo_inicial, int nodo_final) {
    vector<int> distancias(num_nodos_, INT_MAX);
    vector<bool> visitados(num_nodos_, false);

    distancias[nodo_inicial] = 0;

    while (true) {
      // Encontrar el nodo no visitado con la distancia más corta
      int nodo_actual = -1;
      int distancia_minima = INT_MAX;

      for (int i = 0; i < num_nodos_; i++) {
        if (!visitados[i] && distancias[i] < distancia_minima) {
          nodo_actual = i;
          distancia_minima = distancias[i];
        }
      }

      // Si no encontramos ningún nodo no visitado, hemos terminado
      if (nodo_actual == -1) {
        break;
      }

      // Marcar el nodo actual como visitado
      visitados[nodo_actual] = true;

      // Actualizar las distancias a los nodos adyacentes
      for (int vecino : adyacencia_[nodo_actual]) {
        if (!visitados[vecino]) {
          int nueva_distancia = distancias[nodo_actual] + 1;

          if (nueva_distancia < distancias[vecino]) {
            distancias[vecino] = nueva_distancia;
          }
        }
      }
    }

    return distancias[nodo_final];
  }

private:
  int num_nodos_;
  vector<vector<int>> adyacencia_;
};

// Clase para representar un árbol binario de búsqueda
class ArbolBinarioBusqueda {
public:
  // Constructor
  ArbolBinarioBusqueda() {
    raiz_ = nullptr;
  }

  // Insertar un nodo
  void insertar(int valor) {
    raiz_ = insertar_recursivo(raiz_, valor);
  }

  // Buscar un nodo
  bool buscar(int valor) {
    return buscar_recursivo(raiz_, valor);
  }

  // Eliminar un nodo
  void eliminar(int valor) {
    raiz_ = eliminar_recursivo(raiz_, valor);
  }

  // Imprimir el árbol en orden
  void imprimir_ordenado() {
    imprimir_ordenado_recursivo(raiz_);
  }

private:
  struct Nodo {
    int valor;
    Nodo* izquierdo;
    Nodo* derecho;

    Nodo(int valor) {
      this->valor = valor;
      this->izquierdo = nullptr;
      this->derecho = nullptr;
    }
  };

  Nodo* raiz_;

  Nodo* insertar_recursivo(Nodo* nodo, int valor) {
    if (nodo == nullptr) {
      return new Nodo(valor);
    }

    if (valor < nodo->valor) {
      nodo->izquierdo = insertar_recursivo(nodo->izquierdo, valor);
    } else {
      nodo->derecho = insertar_recursivo(nodo->derecho, valor);
    }

    return nodo;
  }

  bool buscar_recursivo(Nodo* nodo, int valor) {
    if (nodo == nullptr) {
      return false;
    }

    if (valor == nodo->valor) {
      return true;
    } else if (valor < nodo->valor) {
      return buscar_recursivo(nodo->izquierdo, valor);
    } else {
      return buscar_recursivo(nodo->derecho, valor);
    }
  }

  Nodo* eliminar_recursivo(Nodo* nodo, int valor) {
    if (nodo == nullptr) {
      return nullptr;
    }

    if (valor == nodo->valor) {
      // Caso 1: el nodo no tiene hijos
      if (nodo->izquierdo == nullptr && nodo->derecho == nullptr) {
        delete nodo;
        return nullptr;
      }
      // Caso 2: el nodo tiene un hijo
      else if (nodo->izquierdo == nullptr) {
        Nodo* nodo_derecho = nodo->derecho;
        delete nodo;
        return nodo_derecho;
      } else if (nodo->derecho == nullptr) {
        Nodo* nodo_izquierdo = nodo->izquierdo;
        delete nodo;