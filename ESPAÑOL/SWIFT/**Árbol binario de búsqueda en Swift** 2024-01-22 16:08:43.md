```swift
// Este código es una implementación de un árbol binario de búsqueda en Swift.

// Un árbol binario de búsqueda es una estructura de datos que almacena datos de forma ordenada y permite realizar búsquedas eficientes.

// Cada nodo del árbol contiene un valor y dos punteros, uno al nodo hijo izquierdo y otro al nodo hijo derecho.

// El árbol se construye añadiendo nuevos nodos en el lugar correcto, de forma que el árbol siempre esté ordenado.

// Para buscar un nodo en el árbol, se empieza en la raíz del árbol y se compara el valor del nodo con el valor del nodo actual.

// Si el valor del nodo es mayor que el valor del nodo actual, se sigue por el puntero derecho.

// Si el valor del nodo es menor que el valor del nodo actual, se sigue por el puntero izquierdo.

// Si el valor del nodo es igual que el valor del nodo actual, se ha encontrado el nodo.

// Este código implementa las siguientes operaciones básicas en un árbol binario de búsqueda:

// - insertar un nuevo nodo
// - buscar un nodo
// - eliminar un nodo
// - recorrer el árbol en orden ascendente
// - recorrer el árbol en orden descendente
// - recorrer el árbol en orden transversal

// La clase `Nodo` representa un nodo del árbol.

class Nodo<T: Comparable> {
  var valor: T
  var hijoIzquierdo: Nodo?
  var hijoDerecho: Nodo?

  init(valor: T) {
    self.valor = valor
    hijoIzquierdo = nil
    hijoDerecho = nil
  }
}

// La clase `ArbolBinarioDeBusqueda` representa un árbol binario de búsqueda.

class ArbolBinarioDeBusqueda<T: Comparable> {
  var raiz: Nodo<T>?

  // Insertar un nuevo nodo en el árbol.

  func insertar(valor: T) {
    if raiz == nil {
      raiz = Nodo(valor: valor)
    } else {
      insertarRecursivo(nodo: raiz!, valor: valor)
    }
  }

  private func insertarRecursivo(nodo: Nodo<T>, valor: T) {
    if valor < nodo.valor {
      if nodo.hijoIzquierdo == nil {
        nodo.hijoIzquierdo = Nodo(valor: valor)
      } else {
        insertarRecursivo(nodo: nodo.hijoIzquierdo!, valor: valor)
      }
    } else {
      if nodo.hijoDerecho == nil {
        nodo.hijoDerecho = Nodo(valor: valor)
      } else {
        insertarRecursivo(nodo: nodo.hijoDerecho!, valor: valor)
      }
    }
  }

  // Buscar un nodo en el árbol.

  func buscar(valor: T) -> Nodo<T>? {
    if raiz == nil {
      return nil
    } else {
      return buscarRecursivo(nodo: raiz!, valor: valor)
    }
  }

  private func buscarRecursivo(nodo: Nodo<T>, valor: T) -> Nodo<T>? {
    if nodo.valor == valor {
      return nodo
    } else if valor < nodo.valor {
      if nodo.hijoIzquierdo == nil {
        return nil
      } else {
        return buscarRecursivo(nodo: nodo.hijoIzquierdo!, valor: valor)
      }
    } else {
      if nodo.hijoDerecho == nil {
        return nil
      } else {
        return buscarRecursivo(nodo: nodo.hijoDerecho!, valor: valor)
      }
    }
  }

  // Eliminar un nodo del árbol.

  func eliminar(valor: T) {
    if raiz == nil {
      return
    } else {
      eliminarRecursivo(nodo: raiz!, valor: valor)
    }
  }

  private func eliminarRecursivo(nodo: Nodo<T>, valor: T) {
    if valor < nodo.valor {
      if nodo.hijoIzquierdo == nil {
        return
      } else {
        eliminarRecursivo(nodo: nodo.hijoIzquierdo!, valor: valor)
      }
    } else if valor > nodo.valor {
      if nodo.hijoDerecho == nil {
        return
      } else {
        eliminarRecursivo(nodo: nodo.hijoDerecho!, valor: valor)
      }
    } else {
      // El nodo a eliminar es el nodo actual.

      // Si el nodo actual no tiene hijos, simplemente se elimina.

      if nodo.hijoIzquierdo == nil && nodo