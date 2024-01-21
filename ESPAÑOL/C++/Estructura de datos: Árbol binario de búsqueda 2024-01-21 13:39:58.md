```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>

using namespace std;

// Estructura para representar un nodo en un árbol binario de búsqueda.
struct nodoArbol {
  int valor;
  nodoArbol *izquierda;
  nodoArbol *derecha;

  nodoArbol(int valor) : valor(valor), izquierda(nullptr), derecha(nullptr) {}
};

// Función para insertar un valor en un árbol binario de búsqueda.
void insertar(nodoArbol *&raiz, int valor) {
  if (raiz == nullptr) {
    raiz = new nodoArbol(valor);
  } else if (valor < raiz->valor) {
    insertar(raiz->izquierda, valor);
  } else {
    insertar(raiz->derecha, valor);
  }
}

// Función para buscar un valor en un árbol binario de búsqueda.
bool buscar(nodoArbol *raiz, int valor) {
  if (raiz == nullptr) {
    return false;
  } else if (raiz->valor == valor) {
    return true;
  } else if (valor < raiz->valor) {
    return buscar(raiz->izquierda, valor);
  } else {
    return buscar(raiz->derecha, valor);
  }
}

// Función para eliminar un valor de un árbol binario de búsqueda.
void eliminar(nodoArbol *&raiz, int valor) {
  if (raiz == nullptr) {
    return;
  } else if (valor < raiz->valor) {
    eliminar(raiz->izquierda, valor);
  } else if (valor > raiz->valor) {
    eliminar(raiz->derecha, valor);
  } else {
    if (raiz->izquierda == nullptr) {
      nodoArbol *temp = raiz;
      raiz = raiz->derecha;
      delete temp;
    } else if (raiz->derecha == nullptr) {
      nodoArbol *temp = raiz;
      raiz = raiz->izquierda;
      delete temp;
    } else {
      nodoArbol *predecesor = raiz->izquierda;
      while (predecesor->derecha != nullptr) {
        predecesor = predecesor->derecha;
      }
      raiz->valor = predecesor->valor;
      eliminar(raiz->izquierda, predecesor->valor);
    }
  }
}

// Función para imprimir un árbol binario de búsqueda en orden.
void imprimirEnOrden(nodoArbol *raiz) {
  if (raiz != nullptr) {
    imprimirEnOrden(raiz->izquierda);
    cout << raiz->valor << " ";
    imprimirEnOrden(raiz->derecha);
  }
}

// Función para imprimir un árbol binario de búsqueda en preorden.
void imprimirPreOrden(nodoArbol *raiz) {
  if (raiz != nullptr) {
    cout << raiz->valor << " ";
    imprimirPreOrden(raiz->izquierda);
    imprimirPreOrden(raiz->derecha);
  }
}

// Función para imprimir un árbol binario de búsqueda en postorden.
void imprimirPostOrden(nodoArbol *raiz) {
  if (raiz != nullptr) {
    imprimirPostOrden(raiz->izquierda);
    imprimirPostOrden(raiz->derecha);
    cout << raiz->valor << " ";
  }
}

// Función para encontrar el valor mínimo en un árbol binario de búsqueda.
int encontrarMinimo(nodoArbol *raiz) {
  if (raiz == nullptr) {
    return -1;
  } else if (raiz->izquierda == nullptr) {
    return raiz->valor;
  } else {
    return encontrarMinimo(raiz->izquierda);
  }
}

// Función para encontrar el valor máximo en un árbol binario de búsqueda.
int encontrarMaximo(nodoArbol *raiz) {
  if (raiz == nullptr) {
    return -1;
  } else if (raiz->derecha == nullptr) {
    return raiz->valor;
  } else {
    return encontrarMaximo(raiz->derecha);
  }
}

// Función para encontrar el valor sucesor de un valor en un árbol binario de búsqueda.
int encontrarSucesor(nodoArbol *raiz, int valor) {
  nodoArbol *actual = raiz;
  nodoArbol *predecesor = nullptr;

  while (actual != nullptr) {
    if (actual->valor == valor) {
      if (actual->derecha != nullptr) {
        return encontrarMinimo(actual->derecha);
      } else {
        return predecesor->valor;
      }
    } else if (actual->valor < valor) {
      actual = actual->derecha;
    } else {
      predecesor = actual;
      actual = actual->izquierda;
    }
  }

  return -1;
}

// Función para encontrar el valor predecesor de un valor en un árbol binario de búsqueda.
int encontrarPredecesor(nodoArbol *raiz, int valor) {
  nodoArbol *actual = raiz;
  nodoArbol *predecesor = nullptr;

  while (actual != nullptr) {
    if (actual->valor == valor) {
      if (actual->izquierda != nullptr) {
        return encontrarMaximo(actual->izquierda);
      } else {
        return predecesor->valor;
      }
    } else if (actual->valor < valor) {
      predecesor = actual;
      actual = actual->derecha;
    } else {
      actual = actual->izquierda;
    }
  }

  return -1;
}

// Función para verificar si un árbol binario de búsqueda es un árbol de búsqueda binario válido.
bool esArbolDeBusquedaBinarioValido(nodoArbol *raiz) {
  if (raiz == nullptr) {
    return true;
  } else {
    return (raiz->izquierda == nullptr || raiz->izquierda->valor < raiz->valor) &&
           (raiz->derecha == nullptr || raiz->derecha->valor > raiz->valor) &&
           esArbolDeBusquedaBinarioValido(raiz->izquierda) &&
           esArbolDeBusquedaBinarioValido(raiz->derecha);
  }
}

// Función para contar el número de nodos en un árbol binario de búsqueda.
int contarNodos(nodoArbol *raiz) {
  if (raiz == nullptr) {
    return 0;
  } else {
    return 1 + contarNodos(raiz->izquierda) + contarNodos(raiz->derecha);
  }
}

// Función para contar el número de hojas en un árbol binario de búsqueda.
int contarHojas(nodoArbol *raiz) {
  if (raiz == nullptr) {
    return 0;
  } else if (raiz->izquierda == nullptr && raiz->derecha == nullptr) {
    return 1;
  } else {
    return contarHojas(raiz->izquierda) + contarHojas(raiz->derecha);
  }
}

// Función para encontrar la altura de un árbol binario de búsqueda.
int encontrarAltura(nodoArbol *raiz) {
  if (raiz == nullptr) {
    return 0;
  } else {
    return max(encontrarAltura(raiz->izquierda), encontrarAltura(raiz->derecha)) + 1;
  }
}

// Función para encontrar el diámetro de un árbol binario de búsqueda.
int encontrarDiametro(nodoArbol *raiz) {
  if (raiz == nullptr) {
    return 0;
  } else {
    return max(encontrarAltura