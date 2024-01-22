```javascript
// **Función de burbuja para ordenar una lista de números**

const bubbleSort = (lista) => {
  let intercambiado = true;
  const largoLista = lista.length;
  while (intercambiado) {
    intercambiado = false;
    for (let i = 0; i < largoLista - 1; i++) {
      if (lista[i] > lista[i + 1]) {
        const temporal = lista[i];
        lista[i] = lista[i + 1];
        lista[i + 1] = temporal;
        intercambiado = true;
      }
    }
  }
  return lista;
};

// **Ejemplo de uso de la función de burbuja**

const lista desordenada = [5, 1, 4, 2, 8];
console.log("Lista desordenada:", lista desordenada);
const lista ordenada = bubbleSort(lista desordenada);
console.log("Lista ordenada:", lista ordenada);


// **Función de búsqueda binaria para encontrar un elemento en una lista ordenada**

const binarySearch = (lista, elemento) => {
  let bajo = 0;
  let alto = lista.length - 1;
  while (bajo <= alto) {
    const medio = Math.floor((bajo + alto) / 2);
    if (lista[medio] === elemento) {
      return medio;
    } else if (lista[medio] < elemento) {
      bajo = medio + 1;
    } else {
      alto = medio - 1;
    }
  }
  return -1;
};

// **Ejemplo de uso de la función de búsqueda binaria**

const lista_ordenada = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const elemento_a_buscar = 5;
const índice_del_elemento = binarySearch(lista_ordenada, elemento_a_buscar);
console.log(`El elemento ${elemento_a_buscar} se encuentra en el índice ${índice_del_elemento}`);


// **Función de árbol binario de búsqueda para almacenar y buscar elementos**

class Nodo {
  constructor(valor) {
    this.valor = valor;
    this.izquierda = null;
    this.derecha = null;
  }
}

class ÁrbolBinarioDeBúsqueda {
  constructor() {
    this.raíz = null;
  }

  insertar(valor) {
    const nuevoNodo = new Nodo(valor);
    if (this.raíz === null) {
      this.raíz = nuevoNodo;
    } else {
      this._insertar(nuevoNodo, this.raíz);
    }
  }

  _insertar(nuevoNodo, nodoActual) {
    if (nuevoNodo.valor < nodoActual.valor) {
      if (nodoActual.izquierda === null) {
        nodoActual.izquierda = nuevoNodo;
      } else {
        this._insertar(nuevoNodo, nodoActual.izquierda);
      }
    } else {
      if (nodoActual.derecha === null) {
        nodoActual.derecha = nuevoNodo;
      } else {
        this._insertar(nuevoNodo, nodoActual.derecha);
      }
    }
  }

  buscar(valor) {
    return this._buscar(valor, this.raíz);
  }

  _buscar(valor, nodoActual) {
    if (nodoActual === null) {
      return false;
    } else if (valor === nodoActual.valor) {
      return nodoActual;
    } else if (valor < nodoActual.valor) {
      return this._buscar(valor, nodoActual.izquierda);
    } else {
      return this._buscar(valor, nodoActual.derecha);
    }
  }
}

// **Ejemplo de uso del árbol binario de búsqueda**

const árbol = new ÁrbolBinarioDeBúsqueda();
árbol.insertar(5);
árbol.insertar(2);
árbol.insertar(8);
árbol.insertar(1);
árbol.insertar(3);
árbol.insertar(7);
árbol.insertar(9);
console.log("Elementos del árbol binario de búsqueda:");
console.log(árbol);
const elemento_a_buscar = 8;
const nodo_encontrado = árbol.buscar(elemento_a_buscar);
console.log(`El nodo con el valor ${elemento_a_buscar} es:`, nodo_encontrado);
```

Explicación:

1. **Función de burbuja:** Esta función implementa el algoritmo de ordenación por burbuja para ordenar una lista de números en orden ascendente.
2. **Función de búsqueda binaria:** Esta función implementa el algoritmo de búsqueda binaria para encontrar un elemento en una lista ordenada en tiempo logarítmico.
3. **Clase Árbol binario de búsqueda:** Esta clase implementa un árbol binario de búsqueda, que es una estructura de datos que se puede utilizar para almacenar y buscar datos de manera eficiente.
4. **Métodos del árbol binario de búsqueda:** La clase `ÁrbolBinarioDeBúsqueda` tiene dos métodos principales: `insertar()` y `buscar()`. El método `insertar()` inserta un nuevo elemento en el árbol, y el método `buscar()` busca un elemento en el árbol.
5. **Ejemplo de uso del árbol binario de búsqueda:** El código crea un nuevo árbol binario de búsqueda, inserta algunos elementos en el árbol y luego busca un elemento en el árbol.