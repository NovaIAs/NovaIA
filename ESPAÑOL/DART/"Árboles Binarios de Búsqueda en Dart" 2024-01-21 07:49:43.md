```dart
// Importar las bibliotecas necesarias
import 'dart:collection';
import 'dart:convert';
import 'dart:io';
import 'dart:math';

// Definir una clase para representar un nodo en un árbol binario de búsqueda
class NodoArbolBinario {
  int valor;
  NodoArbolBinario? izquierdo;
  NodoArbolBinario? derecho;

  NodoArbolBinario(this.valor);
}

// Definir una función para insertar un valor en un árbol binario de búsqueda
void insertarEnArbolBinario(NodoArbolBinario nodo, int valor) {
  if (valor < nodo.valor) {
    if (nodo.izquierdo == null) {
      nodo.izquierdo = NodoArbolBinario(valor);
    } else {
      insertarEnArbolBinario(nodo.izquierdo!, valor);
    }
  } else {
    if (nodo.derecho == null) {
      nodo.derecho = NodoArbolBinario(valor);
    } else {
      insertarEnArbolBinario(nodo.derecho!, valor);
    }
  }
}

// Definir una función para buscar un valor en un árbol binario de búsqueda
bool buscarEnArbolBinario(NodoArbolBinario nodo, int valor) {
  if (nodo == null) {
    return false;
  } else if (valor == nodo.valor) {
    return true;
  } else if (valor < nodo.valor) {
    return buscarEnArbolBinario(nodo.izquierdo, valor);
  } else {
    return buscarEnArbolBinario(nodo.derecho, valor);
  }
}

// Definir una función para eliminar un valor de un árbol binario de búsqueda
NodoArbolBinario? eliminarDeArbolBinario(NodoArbolBinario nodo, int valor) {
  if (nodo == null) {
    return null;
  } else if (valor == nodo.valor) {
    if (nodo.izquierdo == null && nodo.derecho == null) {
      return null;
    } else if (nodo.izquierdo != null && nodo.derecho == null) {
      return nodo.izquierdo;
    } else if (nodo.izquierdo == null && nodo.derecho != null) {
      return nodo.derecho;
    } else {
      NodoArbolBinario? nodoMinimoDerecha = nodo.derecho;
      while (nodoMinimoDerecha!.izquierdo != null) {
        nodoMinimoDerecha = nodoMinimoDerecha.izquierdo;
      }
      nodo.valor = nodoMinimoDerecha.valor;
      nodo.derecho = eliminarDeArbolBinario(nodo.derecho, nodoMinimoDerecha.valor);
      return nodo;
    }
  } else if (valor < nodo.valor) {
    nodo.izquierdo = eliminarDeArbolBinario(nodo.izquierdo, valor);
    return nodo;
  } else {
    nodo.derecho = eliminarDeArbolBinario(nodo.derecho, valor);
    return nodo;
  }
}

// Definir una función para imprimir un árbol binario de búsqueda en orden
void imprimirArbolBinarioEnOrden(NodoArbolBinario nodo) {
  if (nodo != null) {
    imprimirArbolBinarioEnOrden(nodo.izquierdo);
    print(nodo.valor);
    imprimirArbolBinarioEnOrden(nodo.derecho);
  }
}

// Definir una función para imprimir un árbol binario de búsqueda en preorden
void imprimirArbolBinarioEnPreorden(NodoArbolBinario nodo) {
  if (nodo != null) {
    print(nodo.valor);
    imprimirArbolBinarioEnPreorden(nodo.izquierdo);
    imprimirArbolBinarioEnPreorden(nodo.derecho);
  }
}

// Definir una función para imprimir un árbol binario de búsqueda en postorden
void imprimirArbolBinarioEnPostorden(NodoArbolBinario nodo) {
  if (nodo != null) {
    imprimirArbolBinarioEnPostorden(nodo.izquierdo);
    imprimirArbolBinarioEnPostorden(nodo.derecho);
    print(nodo.valor);
  }
}

// Definir una función para contar el número de nodos en un árbol binario de búsqueda
int contarNodosEnArbolBinario(NodoArbolBinario nodo) {
  if (nodo == null) {
    return 0;
  } else {
    return 1 + contarNodosEnArbolBinario(nodo.izquierdo) + contarNodosEnArbolBinario(nodo.dere