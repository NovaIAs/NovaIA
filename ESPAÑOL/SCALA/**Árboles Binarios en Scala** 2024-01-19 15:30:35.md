```scala
// Clase que representa un árbol binario
class ArbolBinario[T] {

  // Atributo que representa el nodo raíz del árbol
  private var raiz: Nodo[T] = null

  // Clase que representa un nodo del árbol
  private class Nodo[T](var dato: T, var izquierdo: Nodo[T], var derecho: Nodo[T])

  // Método para insertar un nuevo nodo en el árbol
  def insertar(dato: T): Unit = {
    // Si el árbol está vacío, el nuevo nodo se convierte en la raíz
    if (raiz == null) {
      raiz = new Nodo[T](dato, null, null)
    } else {
      // Si el árbol no está vacío, se llama a un método recursivo para insertar el nuevo nodo en la posición correcta
      insertarRecursivo(dato, raiz)
    }
  }

  // Método recursivo para insertar un nuevo nodo en el árbol
  private def insertarRecursivo(dato: T, nodo: Nodo[T]): Unit = {
    // Si el dato es menor que el dato del nodo actual, se inserta en el subárbol izquierdo
    if (dato < nodo.dato) {
      if (nodo.izquierdo == null) {
        nodo.izquierdo = new Nodo[T](dato, null, null)
      } else {
        insertarRecursivo(dato, nodo.izquierdo)
      }
    }
    // Si el dato es mayor que el dato del nodo actual, se inserta en el subárbol derecho
    else {
      if (nodo.derecho == null) {
        nodo.derecho = new Nodo[T](dato, null, null)
      } else {
        insertarRecursivo(dato, nodo.derecho)
      }
    }
  }

  // Método para buscar un nodo en el árbol
  def buscar(dato: T): Nodo[T] = {
    // Si el árbol está vacío, se devuelve null
    if (raiz == null) {
      return null
    } else {
      // Si el árbol no está vacío, se llama a un método recursivo para buscar el nodo en la posición correcta
      return buscarRecursivo(dato, raiz)
    }
  }

  // Método recursivo para buscar un nodo en el árbol
  private def buscarRecursivo(dato: T, nodo: Nodo[T]): Nodo[T] = {
    // Si el dato es igual al dato del nodo actual, se devuelve el nodo
    if (dato == nodo.dato) {
      return nodo
    }
    // Si el dato es menor que el dato del nodo actual, se busca en el subárbol izquierdo
    else if (dato < nodo.dato) {
      if (nodo.izquierdo == null) {
        return null
      } else {
        return buscarRecursivo(dato, nodo.izquierdo)
      }
    }
    // Si el dato es mayor que el dato del nodo actual, se busca en el subárbol derecho
    else {
      if (nodo.derecho == null) {
        return null
      } else {
        return buscarRecursivo(dato, nodo.derecho)
      }
    }
  }

  // Método para eliminar un nodo del árbol
  def eliminar(dato: T): Unit = {
    // Si el árbol está vacío, no se puede eliminar ningún nodo
    if (raiz == null) {
      return
    } else {
      // Si el árbol no está vacío, se llama a un método recursivo para eliminar el nodo en la posición correcta
      eliminarRecursivo(dato, raiz)
    }
  }

  // Método recursivo para eliminar un nodo del árbol
  private def eliminarRecursivo(dato: T, nodo: Nodo[T]): Unit = {
    // Si el dato es igual al dato del nodo actual, se elimina el nodo
    if (dato == nodo.dato) {
      // Si el nodo no tiene hijos, se elimina directamente
      if (nodo.izquierdo == null && nodo.derecho == null) {
        nodo = null
      }
      // Si el nodo tiene un solo hijo, se sustituye por su hijo
      else if (nodo.izquierdo != null && nodo.derecho == null) {
        nodo = nodo.izquierdo
      } else if (nodo.izquierdo == null && nodo.derecho != null) {
        nodo = nodo.derecho
      }
      // Si el nodo tiene dos hijos, se sustituye por el nodo más pequeño del subárbol derecho
      else {
        // Se busca el nodo más pequeño del subárbol derecho
        var nodoMasPequeno = nodo.derecho
        while (nodoMasPequeno.izquierdo != null) {
          nodoMasPequeno = nodoMasPequeno.izquierdo
        }

        // Se sustituye el nodo actual por el nodo más pequeño del subárbol derecho
        nodo.dato = nodoMasPequeno.dato
        nodo.derecho = eliminarRecursivo(nodoMasPequeno.dato, nodo.derecho)
      }
    }
    // Si el dato es menor que el dato del nodo actual, se busca en el subárbol izquierdo
    else if (dato < nodo.dato) {
      if (nodo.izquierdo != null) {
        eliminarRecursivo(dato, nodo.izquierdo)
      }
    }
    // Si el dato es mayor que el dato del nodo actual, se busca en el subárbol derecho
    else {
      if (nodo.derecho != null) {
        eliminarRecursivo(dato, nodo.derecho)
      }
    }
  }

  // Método para imprimir el árbol en preorden
  def imprimirPreorden(): Unit = {
    // Si el árbol está vacío, no se imprime nada
    if (raiz == null) {
