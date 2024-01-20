```c++
// Archivo: programa_complejo.cpp

// Librerías necesarias
#include <iostream>
#include <vector>
#include <algorithm>

// Espacio de nombres estándar
using namespace std;

// Función para imprimir un vector de enteros
void imprimir_vector(const vector<int>& v) {
  cout << "[";
  for (int i = 0; i < v.size(); i++) {
    cout << v[i];
    if (i < v.size() - 1) {
      cout << ", ";
    }
  }
  cout << "]" << endl;
}

// Función para ordenar un vector de enteros de forma descendente
void ordenar_vector_descendente(vector<int>& v) {
  sort(v.begin(), v.end(), [](int a, int b) { return a > b; });
}

// Clase para representar un árbol binario de búsqueda
class ArbolBinarioBusqueda {
private:
  struct Nodo {
    int valor;
    Nodo* izquierdo;
    Nodo* derecho;

    Nodo(int valor) : valor(valor), izquierdo(nullptr), derecho(nullptr) {}
  };

  Nodo* raiz;

  // Función recursiva para insertar un nodo en el árbol
  void insertar_nodo(Nodo*& nodo, int valor) {
    if (nodo == nullptr) {
      nodo = new Nodo(valor);
    } else if (valor < nodo->valor) {
      insertar_nodo(nodo->izquierdo, valor);
    } else {
      insertar_nodo(nodo->derecho, valor);
    }
  }

  // Función recursiva para buscar un nodo en el árbol
  bool buscar_nodo(Nodo* nodo, int valor) {
    if (nodo == nullptr) {
      return false;
    } else if (nodo->valor == valor) {
      return true;
    } else if (valor < nodo->valor) {
      return buscar_nodo(nodo->izquierdo, valor);
    } else {
      return buscar_nodo(nodo->derecho, valor);
    }
  }

public:
  ArbolBinarioBusqueda() : raiz(nullptr) {}

  // Función para insertar un nodo en el árbol
  void insertar(int valor) {
    insertar_nodo(raiz, valor);
  }

  // Función para buscar un nodo en el árbol
  bool buscar(int valor) {
    return buscar_nodo(raiz, valor);
  }
};

// Función principal
int main() {
  // Creamos un vector de enteros
  vector<int> v = {1, 3, 5, 2, 4, 6};

  // Imprimimos el vector original
  cout << "Vector original:" << endl;
  imprimir_vector(v);

  // Ordenamos el vector de forma descendente
  ordenar_vector_descendente(v);

  // Imprimimos el vector ordenado
  cout << "Vector ordenado de forma descendente:" << endl;
  imprimir_vector(v);

  // Creamos un árbol binario de búsqueda
  ArbolBinarioBusqueda arbol;

  // Insertamos los elementos del vector en el árbol
  for (int elemento : v) {
    arbol.insertar(elemento);
  }

  // Buscamos un elemento en el árbol
  int elemento_a_buscar = 4;
  bool encontrado = arbol.buscar(elemento_a_buscar);

  // Imprimimos el resultado de la búsqueda
  if (encontrado) {
    cout << "El elemento " << elemento_a_buscar << " fue encontrado en el árbol." << endl;
  } else {
    cout << "El elemento " << elemento_a_buscar << " no fue encontrado en el árbol." << endl;
  }

  return 0;
}
```

Explicación del código:

* Se define una función `imprimir_vector` que imprime un vector de enteros.
* Se define una función `ordenar_vector_descendente` que ordena un vector de enteros de forma descendente.
* Se define una clase `ArbolBinarioBusqueda` que representa un árbol binario de búsqueda.
* En la función `main`, se crea un vector de enteros y se imprime.
* Se ordena el vector de forma descendente y se imprime nuevamente.
* Se crea un árbol binario de búsqueda e insertan los elementos del vector en el árbol.
* Se busca un elemento en el árbol y se imprime el resultado de la búsqueda.

Este código es bastante complejo y cubre varios temas avanzados de programación, como estructuras de datos (vectores y árboles binarios de búsqueda), algoritmos (ordenamiento) y funciones recursivas.