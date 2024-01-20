```scala
// Definición de una clase para representar un árbol binario de búsqueda
class ArbolBinarioBusqueda[T <: Comparable[T]] {

  // Clase interna para representar los nodos del árbol
  private class Nodo(var valor: T, var izquierda: ArbolBinarioBusqueda[T], var derecha: ArbolBinarioBusqueda[T]) {
    override def toString: String = s"$valor"
  }

  // Raíz del árbol
  private var raiz: Nodo = null

  // Insertar un nuevo valor en el árbol
  def insertar(valor: T): Unit = {
    // Si el árbol está vacío, crear la raíz con el nuevo valor
    if (raiz == null) {
      raiz = new Nodo(valor, null, null)
      return
    }

    // Buscar el nodo donde insertar el nuevo valor
    var nodoActual = raiz
    while (true) {
      if (valor.compareTo(nodoActual.valor) < 0) {
        // Si el nuevo valor es menor que el valor actual, ir a la izquierda
        if (nodoActual.izquierda == null) {
          nodoActual.izquierda = new Nodo(valor, null, null)
          return
        } else {
          nodoActual = nodoActual.izquierda
        }
      } else {
        // Si el nuevo valor es mayor o igual que el valor actual, ir a la derecha
        if (nodoActual.derecha == null) {
          nodoActual.derecha = new Nodo(valor, null, null)
          return
        } else {
          nodoActual = nodoActual.derecha
        }
      }
    }
  }

  // Buscar un valor en el árbol
  def buscar(valor: T): Boolean = {
    // Si el árbol está vacío, devolver falso
    if (raiz == null) {
      return false
    }

    // Buscar el nodo que contiene el valor
    var nodoActual = raiz
    while (nodoActual != null) {
      if (valor.compareTo(nodoActual.valor) == 0) {
        // Si el valor actual es igual al valor buscado, devolver verdadero
        return true
      } else if (valor.compareTo(nodoActual.valor) < 0) {
        // Si el valor buscado es menor que el valor actual, ir a la izquierda
        nodoActual = nodoActual.izquierda
      } else {
        // Si el valor buscado es mayor que el valor actual, ir a la derecha
        nodoActual = nodoActual.derecha
      }
    }

    // Si no se encontró el valor, devolver falso
    return false
  }

  // Eliminar un valor del árbol
  def eliminar(valor: T): Unit = {
    // Buscar el nodo que contiene el valor a eliminar
    var nodoActual = raiz
    var padreActual: Nodo = null
    while (nodoActual != null) {
      if (valor.compareTo(nodoActual.valor) == 0) {
        // Si el valor actual es igual al valor a eliminar, salir del bucle
        break
      } else if (valor.compareTo(nodoActual.valor) < 0) {
        // Si el valor a eliminar es menor que el valor actual, ir a la izquierda
        padreActual = nodoActual
        nodoActual = nodoActual.izquierda
      } else {
        // Si el valor a eliminar es mayor que el valor actual, ir a la derecha
        padreActual = nodoActual
        nodoActual = nodoActual.derecha
      }
    }

    // Si el nodo a eliminar no se encontró, devolver
    if (nodoActual == null) {
      return
    }

    // Si el nodo a eliminar no tiene hijos, eliminarlo directamente
    if (nodoActual.izquierda == null && nodoActual.derecha == null) {
      if (padreActual == null) {
        raiz = null
      } else if (padreActual.izquierda == nodoActual) {
        padreActual.izquierda = null
      } else {
        padreActual.derecha = null
      }
      return
    }

    // Si el nodo a eliminar tiene un solo hijo, reemplazarlo por su hijo
    if (nodoActual.izquierda == null) {
      if (padreActual == null) {
        raiz = nodoActual.derecha
      } else if (padreActual.izquierda == nodoActual) {
        padreActual.izquierda = nodoActual.derecha
      } else {
        padreActual.derecha = nodoActual.derecha
      }
      return
    } else if (nodoActual.derecha == null) {
      if (padreActual == null) {
        raiz = nodoActual.izquierda
      } else if (padreActual.izquierda == nodoActual) {
        padreActual.izquierda = nodoActual.izquierda
      } else {
        padreActual.derecha = nodoActual.izquierda
      }
      return
    }

    // Si el nodo a eliminar tiene dos hijos, reemplazarlo por su sucesor inorden
    var sucesor = nodoActual.derecha
    var padreSucesor: Nodo = nodoActual
    while (sucesor.izquierda != null) {
      padreSucesor = sucesor
      sucesor = sucesor.izquierda
    }

    // Reemplazar el nodo a eliminar por su sucesor inorden
    nodoActual.valor = sucesor.valor
    if (padreSucesor == nodoActual) {
      nodoActual.derecha = sucesor.derecha
    } else {
      padreSucesor.izquierda = sucesor.derecha
    }
  }

  // Recorrer el árbol en orden inorden
  def inorden(