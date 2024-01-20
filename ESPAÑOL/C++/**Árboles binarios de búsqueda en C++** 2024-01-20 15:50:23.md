```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>

using namespace std;

// Clase de nodo para representar un nodo de un árbol binario
class Nodo {
public:
  int dato;
  Nodo* izquierdo;
  Nodo* derecho;

  Nodo(int dato) {
    this->dato = dato;
    this->izquierdo = nullptr;
    this->derecho = nullptr;
  }
};

// Clase de árbol binario
class ArbolBinario {
public:
  Nodo* raiz;

  ArbolBinario() {
    this->raiz = nullptr;
  }

  // Función para insertar un nuevo nodo en el árbol binario
  void insertar(int dato) {
    if (this->raiz == nullptr) {
      this->raiz = new Nodo(dato);
    } else {
      insertarRecursivo(this->raiz, dato);
    }
  }

  // Función recursiva para insertar un nuevo nodo en el árbol binario
  void insertarRecursivo(Nodo* nodoActual, int dato) {
    if (dato < nodoActual->dato) {
      if (nodoActual->izquierdo == nullptr) {
        nodoActual->izquierdo = new Nodo(dato);
      } else {
        insertarRecursivo(nodoActual->izquierdo, dato);
      }
    } else {
      if (nodoActual->derecho == nullptr) {
        nodoActual->derecho = new Nodo(dato);
      } else {
        insertarRecursivo(nodoActual->derecho, dato);
      }
    }
  }

  // Función para buscar un nodo en el árbol binario mediante el procedimiento de búsqueda binaria
  Nodo* buscar(int dato) {
    if (this->raiz == nullptr) {
      return nullptr;
    } else {
      return buscarRecursivo(this->raiz, dato);
    }
  }

  // Función recursiva para buscar un nodo en el árbol binario mediante el procedimiento de búsqueda binaria
  Nodo* buscarRecursivo(Nodo* nodoActual, int dato) {
    if (nodoActual == nullptr) {
      return nullptr;
    } else if (nodoActual->dato == dato) {
      return nodoActual;
    } else if (dato < nodoActual->dato) {
      return buscarRecursivo(nodoActual->izquierdo, dato);
    } else {
      return buscarRecursivo(nodoActual->derecho, dato);
    }
  }

  // Función para eliminar un nodo del árbol binario
  void eliminar(int dato) {
    if (this->raiz == nullptr) {
      return;
    } else {
      eliminarRecursivo(this->raiz, dato);
    }
  }

  // Función recursiva para eliminar un nodo del árbol binario
  void eliminarRecursivo(Nodo* nodoActual, int dato) {
    if (nodoActual == nullptr) {
      return;
    } else if (dato < nodoActual->dato) {
      eliminarRecursivo(nodoActual->izquierdo, dato);
    } else if (dato > nodoActual->dato) {
      eliminarRecursivo(nodoActual->derecho, dato);
    } else {
      // Nodo encontrado, eliminar
      if (nodoActual->izquierdo == nullptr && nodoActual->derecho == nullptr) {
        // Nodo hoja, eliminar
        delete nodoActual;
        nodoActual = nullptr;
      } else if (nodoActual->izquierdo == nullptr) {
        // Nodo con un hijo derecho, reemplazar con el hijo derecho
        Nodo* hijoDerecho = nodoActual->derecho;
        delete nodoActual;
        nodoActual = hijoDerecho;
      } else if (nodoActual->derecho == nullptr) {
        // Nodo con un hijo izquierdo, reemplazar con el hijo izquierdo
        Nodo* hijoIzquierdo = nodoActual->izquierdo;
        delete nodoActual;
        nodoActual = hijoIzquierdo;
      } else {
        // Nodo con dos hijos, reemplazar con el nodo más a la izquierda del subárbol derecho
        Nodo* nodoReemplazo = nodoActual->derecho;
        while (nodoReemplazo->izquierdo != nullptr) {
          nodoReemplazo = nodoReemplazo->izquierdo;
        }

        nodoActual->dato = nodoReemplazo->dato;
        eliminarRecursivo(nodoReemplazo, nodoReemplazo->dato);
      }
    }
  }

  // Función para imprimir el árbol binario en orden ascendente
  void imprimirEnOrden() {
    if (this->raiz == nullptr) {
      cout << "El árbol está vacío" << endl;
    } else {
      imprimirEnOrdenRecursivo(this->raiz);
    }
  }

  // Función recursiva para imprimir el árbol binario en orden ascendente
  void imprimirEnOrdenRecursivo(Nodo* nodoActual) {
    if (nodoActual == nullptr) {
      return;
    }

    imprimirEnOrdenRecursivo(nodoActual->izquierdo);
    cout << nodoActual->dato << " ";
    imprimirEnOrdenRecursivo(nodoActual->derecho);
  }
};

int main() {
  ArbolBinario arbol;

  arbol.insertar(50);
  arbol.insertar(30);
  arbol.insertar(20);
  arbol.insertar(40);
  arbol.insertar(70);
  arbol.insertar(60);
  arbol.insertar(80);

  cout << "Árbol binario:" << endl;
  arbol.imprimirEnOrden();

  cout << endl;

  cout << "Buscar nodo con dato 40:" << endl;
  Nodo* nodoBuscado = arbol.buscar(40);
  if (nodoBuscado != nullptr) {
    cout << "Nodo encontrado: " << nodoBuscado->dato << endl;
  } else {
    cout << "Nodo no encontrado" << endl;
  }

  cout << endl;

  cout << "Eliminar nodo con dato 20:" << endl;
  arbol.eliminar(20);

  cout << "Árbol binario después de eliminar el nodo con dato 20:" << endl;
  arbol.imprimirEnOrden();

  return 0;
}
```

Este código implementa un árbol binario de búsqueda en C++. Un árbol binario de búsqueda es una estructura de datos que almacena datos en nodos, y cada nodo tiene un valor, un hijo izquierdo y un hijo derecho. Los nodos se ordenan de tal manera que el valor del hijo izquierdo es menor que el valor del padre, y el valor del hijo derecho es mayor que el valor del padre.

El código primero define la clase `Nodo` que representa un nodo del árbol binario. La clase `Nodo` tiene tres miembros: `dato`, `izquierdo` y `derecho`. El miembro `dato` almacena el valor del nodo, el miembro `izquierdo` es un puntero al hijo izquierdo del nodo, y el miembro `derecho` es un puntero al hijo derecho del nodo.

A continuación, el código define la clase `ArbolBinario` que representa un árbol binario de búsqueda. La clase `ArbolBinario` tiene un miembro `raiz` que es un puntero al nodo raíz del árbol.

El código también define varias funciones que se pueden utilizar para trabajar con el árbol binario de búsqueda. Estas funciones incluyen:

* `insertar()`: Esta función se utiliza para insertar un nuevo nodo en el árbol binario de búsqueda.
* `buscar()`: Esta función se utiliza para buscar un nodo en el árbol binario de búsqueda.
* `eliminar()`: Esta función se utiliza para eliminar un nodo del árbol binario de búsqueda.
* `imprimirEnOrden()`: Esta función se utiliza para imprimir el árbol binario de búsqueda en orden ascendente.

Finalmente, el código incluye una función `main()` que se utiliza para crear un árbol binario de búsqueda, insertar algunos nodos en el árbol, buscar un nodo en el árbol, eliminar un nodo del árbol e imprimir el árbol en orden ascendente.