```javascript
// Este código implementa un árbol binario en JavaScript.
// Un árbol binario es una estructura de datos que almacena elementos en nodos,
// cada nodo tiene un valor y dos ramas, una rama izquierda y una rama derecha.

class Nodo {
  constructor(valor) {
    this.valor = valor;
    this.izquierda = null;
    this.derecha = null;
  }
}

class ArbolBinario {
  constructor() {
    this.raiz = null;
  }

  // Método para insertar un elemento en el árbol.
  insertar(valor) {
    // Si el árbol está vacío, el nuevo elemento se convierte en la raíz.
    if (this.raiz === null) {
      this.raiz = new Nodo(valor);
    } else {
      // Si el árbol no está vacío, el nuevo elemento se inserta en la posición correcta,
      // utilizando un recorrido en profundidad.
      this._insertarRecursivo(valor, this.raiz);
    }
  }

  _insertarRecursivo(valor, nodoActual) {
    // Si el valor es menor que el valor del nodo actual, se inserta en la rama izquierda.
    if (valor < nodoActual.valor) {
      // Si la rama izquierda es nula, el nuevo elemento se convierte en la rama izquierda.
      if (nodoActual.izquierda === null) {
        nodoActual.izquierda = new Nodo(valor);
      } else {
        // Si la rama izquierda no es nula, se llama al método recursivamente para insertarlo en la rama izquierda.
        this._insertarRecursivo(valor, nodoActual.izquierda);
      }
    } else {
      // Si el valor es mayor que el valor del nodo actual, se inserta en la rama derecha.
      if (valor > nodoActual.valor) {
        // Si la rama derecha es nula, el nuevo elemento se convierte en la rama derecha.
        if (nodoActual.derecha === null) {
          nodoActual.derecha = new Nodo(valor);
        } else {
          // Si la rama derecha no es nula, se llama al método recursivamente para insertarlo en la rama derecha.
          this._insertarRecursivo(valor, nodoActual.derecha);
        }
      }
    }
  }

  // Método para buscar un elemento en el árbol.
  buscar(valor) {
    // Si el árbol está vacío, se devuelve null.
    if (this.raiz === null) {
      return null;
    } else {
      // Si el árbol no está vacío, el elemento se busca en la posición correcta,
      // utilizando un recorrido en profundidad.
      return this._buscarRecursivo(valor, this.raiz);
    }
  }

  _buscarRecursivo(valor, nodoActual) {
    // Si el valor es igual al valor del nodo actual, se devuelve el nodo actual.
    if (valor === nodoActual.valor) {
      return nodoActual;
    } else {
      // Si el valor es menor que el valor del nodo actual, se busca en la rama izquierda.
      if (valor < nodoActual.valor) {
        // Si la rama izquierda es nula, se devuelve null.
        if (nodoActual.izquierda === null) {
          return null;
        } else {
          // Si la rama izquierda no es nula, se llama al método recursivamente para buscarlo en la rama izquierda.
          return this._buscarRecursivo(valor, nodoActual.izquierda);
        }
      } else {
        // Si el valor es mayor que el valor del nodo actual, se busca en la rama derecha.
        if (nodoActual.derecha === null) {
          return null;
        } else {
          // Si la rama derecha no es nula, se llama al método recursivamente para buscarlo en la rama derecha.
          return this._buscarRecursivo(valor, nodoActual.derecha);
        }
      }
    }
  }

  // Método para eliminar un elemento del árbol.
  eliminar(valor) {
    // Si el árbol está vacío, se devuelve null.
    if (this.raiz === null) {
      return null;
    } else {
      // Si el árbol no está vacío, el elemento se elimina en la posición correcta,
      // utilizando un recorrido en profundidad.
      this._eliminarRecursivo(valor, this.raiz);
    }
  }

  _eliminarRecursivo(valor, nodoActual) {
    // Si el valor es igual al valor del nodo actual, se elimina el