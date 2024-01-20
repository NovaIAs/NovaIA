```java
// Este código implementa un árbol binario de búsqueda en Java.
// Un árbol binario de búsqueda es una estructura de datos en la que cada nodo tiene como máximo dos nodos hijos,
// uno a la izquierda y otro a la derecha.
// Los elementos del árbol se encuentran ordenados de menor a mayor, de forma que el elemento más pequeño se encuentra en el nodo raíz,
// y los elementos más grandes se encuentran en los nodos más profundos.

public class ArbolBinarioDeBusqueda {

  private Nodo raiz;

  public ArbolBinarioDeBusqueda() {
    this.raiz = null;
  }

  // Este método añade un nuevo elemento al árbol.
  public void insertar(int elemento) {
    Nodo nuevoNodo = new Nodo(elemento);
    insertarNodo(this.raiz, nuevoNodo);
  }

  // Este método añade un nuevo nodo al árbol de forma recursiva.
  private void insertarNodo(Nodo nodoActual, Nodo nuevoNodo) {
    if (nodoActual == null) {
      this.raiz = nuevoNodo;
    } else {
      if (nuevoNodo.getElemento() < nodoActual.getElemento()) {
        insertarNodo(nodoActual.getIzquierda(), nuevoNodo);
      } else {
        insertarNodo(nodoActual.getDerecha(), nuevoNodo);
      }
    }
  }

  // Este método busca un elemento en el árbol.
  public boolean buscar(int elemento) {
    return buscarNodo(this.raiz, elemento);
  }

  // Este método busca un nodo en el árbol de forma recursiva.
  private boolean buscarNodo(Nodo nodoActual, int elemento) {
    if (nodoActual == null) {
      return false;
    } else if (nodoActual.getElemento() == elemento) {
      return true;
    } else if (elemento < nodoActual.getElemento()) {
      return buscarNodo(nodoActual.getIzquierda(), elemento);
    } else {
      return buscarNodo(nodoActual.getDerecha(), elemento);
    }
  }

  // Este método elimina un elemento del árbol.
  public void eliminar(int elemento) {
    this.raiz = eliminarNodo(this.raiz, elemento);
  }

  // Este método elimina un nodo del árbol de forma recursiva.
  private Nodo eliminarNodo(Nodo nodoActual, int elemento) {
    if (nodoActual == null) {
      return null;
    } else if (elemento < nodoActual.getElemento()) {
      nodoActual.setIzquierda(eliminarNodo(nodoActual.getIzquierda(), elemento));
    } else if (elemento > nodoActual.getElemento()) {
      nodoActual.setDerecha(eliminarNodo(nodoActual.getDerecha(), elemento));
    } else {
      if (nodoActual.getIzquierda() == null) {
        return nodoActual.getDerecha();
      } else if (nodoActual.getDerecha() == null) {
        return nodoActual.getIzquierda();
      } else {
        Nodo nodoReemplazo = encontrarNodoMinimo(nodoActual.getDerecha());
        nodoActual.setElemento(nodoReemplazo.getElemento());
        nodoActual.setDerecha(eliminarNodo(nodoActual.getDerecha(), nodoReemplazo.getElemento()));
      }
    }
    return nodoActual;
  }

  // Este método encuentra el nodo mínimo de un árbol.
  private Nodo encontrarNodoMinimo(Nodo nodoActual) {
    while (nodoActual.getIzquierda() != null) {
      nodoActual = nodoActual.getIzquierda();
    }
    return nodoActual;
  }

  // Este método imprime el árbol en consola.
  public void imprimir() {
    imprimirNodo(this.raiz);
  }

  // Este método imprime un nodo y sus subnodos de forma recursiva.
  private void imprimirNodo(Nodo nodoActual) {
    if (nodoActual != null) {
      System.out.println(nodoActual.getElemento());
      imprimirNodo(nodoActual.getIzquierda());
      imprimirNodo(nodoActual.getDerecha());
    }
  }

  // Este método devuelve el número de elementos del árbol.
  public int size() {
    return sizeNodo(this.raiz);
  }

  // Este método devuelve el número de elementos de un nodo y sus subnodos de forma recursiva.
  private int sizeNodo(Nodo nodoActual) {
    if (nodoActual == null) {
      return 0;
    } else {
      return 1 + sizeNodo(nodoActual.getIzquierda()) + sizeNodo(nodoActual.getDerecha());
    }
  }

  // Este método devuelve la altura del árbol.
  public int height() {
    return heightNodo(this.raiz);